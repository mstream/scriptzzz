import { expect } from '@wdio/globals'
import MainPage from '../pageobjects/main.page.js'

describe('My Login application', () => {
  it('should open mainpage', async () => {
    await MainPage.open()
    await expect(MainPage.canvas).toBeExisting()
    await expect(MainPage.editor).toBeExisting()
  })
})

