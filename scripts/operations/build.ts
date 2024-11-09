import { runRepoCommand } from "../common.ts"

export default async function() {
  await runRepoCommand("npm", "ci")
  await runRepoCommand("spago", "install")
  await runRepoCommand("spago", "test")
}
