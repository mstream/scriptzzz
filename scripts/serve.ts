#! /usr/bin/env -S nix develop --command deno run --allow-env --allow-net --allow-read --allow-run --allow-write

import * as async from "@std/async"
import { runScript } from "./common.ts"
import build from "./operations/build.ts"
import serve from "./operations/serve.ts"

runScript(async () => {
  await build()
  const { host, port, stop } = await serve()
  console.info(`Development server runs at http://${host}:${port}`)
  console.info("Ctrl+C to stop")
  let scheduledToStop = false
  Deno.addSignalListener("SIGINT", () => scheduledToStop = true)
  while (!scheduledToStop) {
    await async.delay(500)
  }
  await stop()
}) 
