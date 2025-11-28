import util from 'node:util'
import { exec as _exec } from 'node:child_process'

const exec = util.promisify(_exec);

export const _simpleSuccessPromise = async () => {
  await new Promise(res => setTimeout(res, 2000))
  return "waited 2 seconds"
}

export const _spawnAsync = async () => {
  let out = ""
  try {
    const { stdout, stderr } = await exec('notecho heres what we echoed');
    out = stdout
  } catch (e) {
    console.log("Caught this in JS")
    throw e
  }
  return out
}
