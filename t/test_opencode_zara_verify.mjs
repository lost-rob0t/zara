import test from 'node:test';
import assert from 'node:assert/strict';
import { createVerifyGate, createVerificationHooks, verificationBaseRef } from '../.opencode/lib/zara-verify.mjs';

function fixture() {
  let state = 'source-a';
  let policy = 'policy-a';
  let time = 100;
  const report = () => ({protocol:'ZARA-VERIFY/1', scope:'local', verdict:'verified',
    source: {identity:state}, created_ms:time, ttl_ms:600000, model_calls:0, provider_calls:0,
    merge_authorized:false, reasons:[], expert:{expert_id:'zara:verifier',operation:'verify.assert',verdict:'succeeded',data:{verified:true},usage:{model_calls:0,provider_calls:0}}});
  const gate = createVerifyGate({
    snapshot:async () => ({identity:state}), policyDigest:async () => policy,
    execute:async operation => operation === 'run' ? report() : {required:['repository']},
    now:() => time,
  });
  return {gate, setState:x=>state=x, setPolicy:x=>policy=x, setTime:x=>time=x};
}

test('release verifier pins the trusted 0.3.x base', () => {
  assert.equal(verificationBaseRef(), 'origin/release/0.3.x');
});

test('completion requires a host-observed run', async () => {
  const {gate}=fixture(); gate.invalidate();
  await assert.rejects(gate.assert('s1'), /verification_required/);
  await gate.invoke('plan','s1');
  await assert.rejects(gate.assert('s1'), /verification_required/);
  await gate.invoke('run','s1'); await gate.assert('s1');
});
test('receipt cannot cross sessions', async () => {
  const {gate}=fixture(); await gate.invoke('run','s1');
  await assert.rejects(gate.assert('s2'), /verification_required/);
});
test('source mutation and ABA invalidate receipt', async () => {
  const {gate,setState}=fixture(); await gate.invoke('run','s1');
  setState('source-b'); await assert.rejects(gate.assert('s1'),/stale_source/);
  setState('source-a'); gate.invalidate();
  await assert.rejects(gate.assert('s1'),/stale_generation/);
});
test('expired receipt and policy edits fail closed', async () => {
  const {gate,setTime,setPolicy}=fixture(); await gate.invoke('run','s1');
  setTime(700000); await assert.rejects(gate.assert('s1'),/expired/);
  setTime(100); setPolicy('policy-b');
  await assert.rejects(gate.assert('s1'),/verifier_changed/);
});
test('unsupported operations cannot inject gate commands', async () => {
  const {gate}=fixture(); await assert.rejects(gate.invoke('sh -c yes','s1'),/invalid_operation/);
});
test('forged positive return is rejected', async () => {
  for (const payload of [{verified:true}, {protocol:'ZARA-VERIFY/1',verdict:'verified'}]) {
    const gate=createVerifyGate({snapshot:async()=>({}),policyDigest:async()=>'p',
      execute:async()=>payload, now:()=>10});
    await gate.invoke('run','s');
    await assert.rejects(gate.assert('s'), /invalid_receipt/);
  }
});
test('provider-backed receipt cannot satisfy zero-model verifier', async () => {
  const payload={protocol:'ZARA-VERIFY/1',scope:'local',verdict:'verified',source:{},
    created_ms:10,ttl_ms:100,model_calls:0,provider_calls:0,merge_authorized:false,reasons:[],
    expert:{expert_id:'zara:verifier',operation:'verify.assert',verdict:'succeeded',
      data:{verified:true},usage:{model_calls:0,provider_calls:1}}};
  const gate=createVerifyGate({snapshot:async()=>({}),policyDigest:async()=>'p',
    execute:async()=>payload,now:()=>10});
  await gate.invoke('run','s');
  await assert.rejects(gate.assert('s'),/invalid_receipt/);
});
test('top-level provider accounting is authoritative', async () => {
  for (const value of [undefined, 1, false]) {
    const payload={protocol:'ZARA-VERIFY/1',scope:'local',verdict:'verified',source:{},
      created_ms:10,ttl_ms:100,model_calls:0,provider_calls:0,merge_authorized:false,reasons:[],
      expert:{expert_id:'zara:verifier',operation:'verify.assert',verdict:'succeeded',
        data:{verified:true},usage:{model_calls:0,provider_calls:0}}};
    if (value === undefined) delete payload.provider_calls; else payload.provider_calls=value;
    const gate=createVerifyGate({snapshot:async()=>({}),policyDigest:async()=>'p',
      execute:async()=>payload,now:()=>10});
    await gate.invoke('run','s');
    await assert.rejects(gate.assert('s'),/invalid_receipt/);
  }
});
test('mutation while running rejects stale output before receipt admission', async () => {
  let release;
  const waiting=new Promise(resolve=>{release=resolve;});
  const gate=createVerifyGate({snapshot:async()=>({}),policyDigest:async()=>'p',
    execute:async()=>{await waiting;return {protocol:'ZARA-VERIFY/1',scope:'local',
      verdict:'verified',source:{},created_ms:10,ttl_ms:100,model_calls:0,provider_calls:0,
      merge_authorized:false,reasons:[],expert:{expert_id:'zara:verifier',operation:'verify.assert',verdict:'succeeded',data:{verified:true},usage:{model_calls:0,provider_calls:0}}};},now:()=>10});
  const pending=gate.invoke('run','s');
  await new Promise(resolve=>setTimeout(resolve,0));
  gate.invalidate(); release();
  await assert.rejects(pending,/stale_generation/);
  await assert.rejects(gate.assert('s'),/verification_required/);
});
test('no concurrent verifier process storms', async () => {
  let release; const wait=new Promise(resolve=>{release=resolve;});
  const gate=createVerifyGate({snapshot:async()=>({}),policyDigest:async()=>'p',
    execute:async()=>{await wait;return {};},now:()=>10});
  const pending=gate.invoke('run','s1');
  await new Promise(resolve=>setTimeout(resolve,0));
  await assert.rejects(gate.invoke('run','s2'),/verification_busy/);
  release(); await pending;
});

test('actual hooks reject completed todos and replace unverified completion', async () => {
  const {gate}=fixture(); const hooks=createVerificationHooks(gate);
  await gate.invoke('run','s');
  await hooks['tool.execute.before']({tool:'edit',sessionID:'s'}, {args:{}});
  await assert.rejects(hooks['tool.execute.before']({tool:'todowrite',sessionID:'s'},
    {args:{todos:[{status:'completed'}]}}),/stale_generation/);
  const output={text:'Everything is green!'};
  await hooks['experimental.text.complete']({sessionID:'s'},output);
  assert.match(output.text,/BLOCKED/); assert.doesNotMatch(output.text,/Everything is green/);
  await gate.invoke('run','s');
  await hooks['tool.execute.before']({tool:'todowrite',sessionID:'s'},
    {args:{todos:[{status:'completed'}]}});
  const verified={text:'Verified locally.'};
  await hooks['experimental.text.complete']({sessionID:'s'},verified);
  assert.equal(verified.text,'Verified locally.');
});
test('hooks propagate source edits across subagent sessions', async () => {
  const {gate}=fixture(); const hooks=createVerificationHooks(gate);
  await gate.invoke('run','parent');
  await hooks['tool.execute.before']({tool:'bash',sessionID:'child'},{args:{command:'anything'}});
  await assert.rejects(gate.assert('parent'),/stale_generation/);
});
test('caller cannot rewrite a stored receipt by mutating returned object',async () => {
  const {gate}=fixture(); const result=await gate.invoke('run','s');
  result.verdict='failed'; await gate.assert('s');
});
test('cancelled operations cannot mint receipts',async () => {
  const {gate}=fixture(); const controller=new AbortController(); controller.abort();
  await assert.rejects(gate.invoke('run','s',controller.signal),/cancelled/);
  await assert.rejects(gate.assert('s'),/verification_required/);
});
