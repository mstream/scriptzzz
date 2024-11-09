import * as esbuild from "esbuild"
import { repoDir } from "../common.ts"

type Result = {
  host: string,
  port: number,
  stop: () => Promise<void>
}

export default async function(): Promise<Result> {
  const context = await esbuild.context({
    absWorkingDir: repoDir,
    bundle: true,
    entryPoints: ['index.js'],
    outfile: 'dist/index.js',
  })
  const { host, port } = await context.serve({ servedir: 'dist' })
  return { host, port, stop: context.dispose }
}
