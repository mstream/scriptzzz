#! /usr/bin/env -S nix develop --command deno run --allow-run

import { runScript } from "./common.ts"
import build from "./operations/build.ts"

runScript(async () => {
  await build()
}) 
