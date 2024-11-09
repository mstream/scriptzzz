import Page from './page.js'

class MainPage extends Page {

  get canvas() { return $('#canvas') }
  get editor() { return $('#editor') }

  async open() {
    await super.open('')
  }
}

export default new MainPage()
