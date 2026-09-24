import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { readFile } from 'node:fs/promises';
import { createHash } from 'node:crypto';
import path from 'node:path';

const executeFile = promisify(execFile);
const protocol = 'ZARA-VERIFY/1';
const releaseBaseRef = 'origin/release/0.3.x';
const protectedPaths = [
  'verification/zara_verify_runner.py', 'verification/zara_verify.pl',
  'verification/zara_verifier_expert.py', 'contracts/zara-verify-v1/spec.json',
  '.opencode/plugins/zara-verify.js', '.opencode/lib/zara-verify.mjs',
];

export function verificationBaseRef() {
  return releaseBaseRef;
}

export function makeRunner(root) {
  async function execute(operation, session, signal) {
    const args = [path.join(root, 'verification/zara_verify_runner.py'), operation,
      '--root', root, '--base', releaseBaseRef, '--session', session];
    let output;
    try {
      const result = await executeFile('python3', args, {
        cwd:root, encoding:'utf8', maxBuffer:2*1024*1024, timeout:7300000,
        signal, killSignal:'SIGINT',
        env:{...process.env, PYTHONDONTWRITEBYTECODE:'1'},
      });
      output = result.stdout;
    } catch (error) {
      if (signal?.aborted) throw new Error('verifier_cancelled');
      if (error.code !== 2 || typeof error.stdout !== 'string') throw new Error('verifier_process_failed');
      output = error.stdout;
    }
    const result = JSON.parse(output);
    if (result.protocol !== protocol) throw new Error('incompatible_verifier');
    return result;
  }
  return {
    execute,
    snapshot:async session => {
      const result = await execute('snapshot', session);
      if (!result.source) throw new Error('source_observation_failed');
      return result.source;
    },
    policyDigest:async () => {
      const hash = createHash('sha256');
      for (const name of protectedPaths) {
        hash.update(name); hash.update('\0'); hash.update(await readFile(path.join(root,name))); hash.update('\0');
      }
      return hash.digest('hex');
    },
  };
}

export function createVerifyGate({execute, snapshot, policyDigest, now=Date.now}) {
  const sessions = new Map();
  let epoch = 0;
  let busy = false;
  const pin = policyDigest();
  async function checkPolicy() {
    if (await pin !== await policyDigest()) throw new Error('verifier_changed_restart_required');
  }
  async function assertCurrent(session) {
    await checkPolicy();
    const record = sessions.get(session);
    if (!record) throw new Error('verification_required');
    if (record.epoch !== epoch) throw new Error('stale_generation');
    const report = record.report;
    if (!report || report.protocol !== protocol || report.scope !== 'local' ||
        report.verdict !== 'verified' || report.model_calls !== 0 || report.provider_calls !== 0 ||
        report.merge_authorized !== false || !Array.isArray(report.reasons) || report.reasons.length ||
        !Number.isSafeInteger(report.created_ms) || !Number.isSafeInteger(report.ttl_ms) ||
        report.ttl_ms <= 0 || report.ttl_ms > 600000 ||
        report.expert?.expert_id !== 'zara:verifier' || report.expert?.operation !== 'verify.assert' ||
        report.expert?.verdict !== 'succeeded' || report.expert?.data?.verified !== true ||
        report.expert?.usage?.model_calls !== 0 || report.expert?.usage?.provider_calls !== 0) {
      throw new Error('invalid_receipt');
    }
    const at = now();
    if (report.created_ms > at || at >= report.created_ms + report.ttl_ms) throw new Error('expired_receipt');
    const source = await snapshot(session);
    if (JSON.stringify(report.source) !== JSON.stringify(source)) throw new Error('stale_source');
    if (record.epoch !== epoch) throw new Error('stale_generation');
    await checkPolicy();
    if (record.epoch !== epoch) throw new Error('stale_generation');
    return structuredClone(report);
  }
  return {
    invalidate() { epoch += 1; },
    assert:assertCurrent,
    async invoke(operation, session, signal) {
      if (!['plan','run','status'].includes(operation)) throw new Error('invalid_operation');
      if (typeof session !== 'string' || !session || session.length > 128) throw new Error('invalid_session');
      if (signal?.aborted) throw new Error('verifier_cancelled');
      await checkPolicy();
      if (operation === 'status') {
        try { return await assertCurrent(session); }
        catch (error) { return {protocol,verdict:'blocked',reasons:[error.message]}; }
      }
      if (busy) throw new Error('verification_busy');
      busy = true;
      const admittedEpoch = epoch;
      try {
        const report = await execute(operation, session, signal);
        await checkPolicy();
        if (signal?.aborted) throw new Error('verifier_cancelled');
        if (admittedEpoch !== epoch) throw new Error('stale_generation');
        if (operation === 'run') {
          if (sessions.size >= 64 && !sessions.has(session)) sessions.delete(sessions.keys().next().value);
          sessions.set(session, {epoch:admittedEpoch,report:structuredClone(report)});
        }
        return report;
      } finally { busy = false; }
    },
  };
}


export function createVerificationHooks(gate) {
  let needsVerification = false;
  const readOnly = new Set(['read', 'glob', 'grep', 'list', 'question', 'zara_verify', 'todowrite']);
  return {
    'tool.execute.before':async (input, output) => {
      if (input.tool === 'zara_verify') needsVerification = true;
      if (input.tool === 'todowrite' && output.args.todos?.some(todo=>todo.status==='completed')) {
        await gate.assert(input.sessionID);
      }
      if (!readOnly.has(input.tool)) {
        needsVerification = true;
        gate.invalidate();
      }
    },
    event:async ({event}) => {
      if (event.type === 'file.edited') { needsVerification = true; gate.invalidate(); }
    },
    'experimental.chat.system.transform':async (_input, output) => {
      output.system.push('ZARA-VERIFY/1 is mandatory for Zara implementation work. Use zara_verify plan before editing and run after the final mutation/commit. Only a current Core verifier-expert receipt permits verified completion or completed todos. Missing, skipped, stale, cancelled and unavailable gates are NOT green. Never submit invented evidence. Local verification does not authorize merge or release.');
    },
    'experimental.text.complete':async (input, output) => {
      if (!needsVerification) return;
      try { await gate.assert(input.sessionID); }
      catch (error) {
        output.text = `ZARA-VERIFY/1: BLOCKED (${error.message}). Changes are not verified. Run zara_verify and resolve its failed or missing evidence. No merge or release is authorized.`;
      }
    },
  };
}
