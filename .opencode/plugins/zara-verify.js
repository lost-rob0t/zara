import { tool } from '@opencode-ai/plugin';
import { createVerifyGate, createVerificationHooks, makeRunner } from '../lib/zara-verify.mjs';

export const ZaraVerifyPlugin = async ({worktree, directory}) => {
  const gate = createVerifyGate(makeRunner(worktree || directory));
  return {
    ...createVerificationHooks(gate),
    tool: {
      zara_verify: tool({
        description:'Mandatory Zara verifier. Prolog chooses all gates; Core verifier expert asserts the result. No caller-supplied evidence or commands.',
        args:{operation:tool.schema.enum(['plan','run','status'])},
        async execute({operation}, context) {
          if (operation === 'run') await context.ask({permission:'zara_verify',patterns:['run'],always:['run'],metadata:{scope:'local',effects:'run repository verification commands'}});
          return JSON.stringify(await gate.invoke(operation,context.sessionID,context.abort));
        },
      }),
    },
  };
}