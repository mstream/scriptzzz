#! /usr/bin/env -S nix develop --command deno run --allow-env --allow-net --allow-read --allow-run --allow-sys --allow-write

import { runScript } from "./common.ts"
import build from "./operations/build.ts"
import serve from "./operations/serve.ts"
import test from "./operations/test.ts"

runScript(async () => {
  await build()
  const { host, port, stop } = await serve()
  try {
    await test({ host, port })
  } finally {
    await stop()
  }
}) 
