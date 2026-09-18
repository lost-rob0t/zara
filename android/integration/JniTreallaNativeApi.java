package ai.zara.app.prolog;

import java.nio.file.Files;
import java.nio.file.Path;

public final class JniTreallaNativeApi {
    public native boolean initialize(String path);
    public native boolean consult(String path);
    public native String[] evaluate(String query);
    public native void shutdown();

    public static void main(String[] args) throws Exception {
        System.load(args[0]);
        JniTreallaNativeApi api = new JniTreallaNativeApi();
        String query = "zara_policy:advise_codes([97,108,108,32,116,101,115,116,115,32,112,97,115,115], Result)";
        Path config = Files.createTempFile("zara-policy-off-", ".pl");
        Files.writeString(config, ":- multifile zara_policy:option/2.\nzara_policy:option(mode,off).\n");
        try {
            for (int cycle = 0; cycle < 3; cycle++) {
                if (!api.initialize(args[1])) throw new AssertionError("Native policy did not load");
                try {
                    String[] result = api.evaluate(query);
                    if (result.length != 1) throw new AssertionError("Expected one native policy report");
                    String report = result[0].replace(" ", "");
                    if (!report.startsWith("[1,") || !report.endsWith("]")) {
                        throw new AssertionError("JNI policy wire mismatch: " + report);
                    }
                    if (!api.consult(config.toString())) throw new AssertionError("Native user config did not load");
                    result = api.evaluate(query);
                    if (result.length != 1 || !result[0].replace(" ", "").equals("[0]")) {
                        throw new AssertionError("Native disabled policy returned advice");
                    }
                } finally { api.shutdown(); }
            }
        } finally { Files.deleteIfExists(config); }
        System.out.println("JNI policy consult/evaluate/restart checks passed");
    }
}
