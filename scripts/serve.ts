#! /usr/bin/env -S nix develop --command deno run --allow-env --allow-net --allow-read --allow-run --allow-write

import { runScript } from "./common.ts"
import build from "./operations/build.ts"
import serve from "./operations/serve.ts"

runScript(async () => {
  await build()
  const { host, port } = await serve()
  console.info(`Development server runs at http://${host}:${port}`)
}) 
