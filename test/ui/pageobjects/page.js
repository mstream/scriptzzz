import { browser } from '@wdio/globals'

export default class Page {
  open(path) {
    return browser.url(`http://127.0.0.1:8000/${path}`)
  }
}
