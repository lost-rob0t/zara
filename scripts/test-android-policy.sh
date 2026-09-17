#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
: "${ZARA_TREALLA_SOURCE_DIR:?Pinned Trealla source is required}"
for tool in swipl make gcc python3 javac java timeout; do command -v "$tool" >/dev/null; done
policy="$root/android/app/src/main/assets/prolog/policy"
report="$root/android/app/build/reports/policy-parity"
mkdir -p "$report"
python3 - "$policy/defaults.pl" <<'PY'
import hashlib, pathlib, sys
body = pathlib.Path(sys.argv[1]).read_bytes()
actual = hashlib.sha1(b'blob ' + str(len(body)).encode() + b'\0' + body).hexdigest()
assert actual == '2ffb6c65558e004080203c1dafb74e41208165f0', actual
PY
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
cp -R "$ZARA_TREALLA_SOURCE_DIR" "$work/trealla"
chmod -R u+w "$work/trealla"
bash "$root/android/patch-trealla-module-path.sh" "$work/trealla"
make -C "$work/trealla" -f GNUmakefile -j2 CC='gcc -fPIC' \
    NOSSL=1 NOFFI=1 NOTHREADS=1 NOTTY=1 NONETWORK=1 tpl libtrealla.a \
    >"$report/build.log" 2>&1
cd "$root"
timeout 90 swipl -q -f none -s android/integration/policy_driver.pl -g policy_driver_main \
    >"$report/swi.stdout" 2>"$report/swi.stderr"
timeout 90 "$work/trealla/tpl" -q -f android/integration/policy_driver.pl -g policy_driver_main \
    >"$report/trealla.stdout" 2>"$report/trealla.stderr"
diff -u "$report/swi.stdout" "$report/trealla.stdout" >"$report/parity.diff"
jdk="${JAVA_HOME:-$(dirname "$(dirname "$(readlink -f "$(command -v javac)")")")}"
gcc -shared -fPIC -pthread -I"$jdk/include" -I"$jdk/include/linux" \
    -I"$work/trealla/src" android/app/src/main/cpp/zara_trealla_jni.c \
    "$work/trealla/libtrealla.a" -lm -ldl -o "$work/libzara_trealla.so"
javac -d "$work/classes" android/integration/JniTreallaNativeApi.java
timeout 30 java -cp "$work/classes" ai.zara.app.prolog.JniTreallaNativeApi \
    "$work/libzara_trealla.so" "$policy/policy.pl" | tee "$report/jni.log"
echo "Native output policy parity and JNI gate passed"
