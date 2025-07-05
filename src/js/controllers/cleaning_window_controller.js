import { Controller } from "@hotwired/stimulus"

export default class extends Controller {
  static targets = ["grid", "button"]
  static values = { 
    adjustText: String,
    hideText: String
  }

  connect() {
    console.log("CleaningWindow controller connected")
  }

  toggle() {
    const grid = this.gridTarget
    
    if (grid.classList.contains('hidden')) {
      grid.classList.remove('hidden')
      this.buttonTarget.textContent = this.hideTextValue
    } else {
      grid.classList.add('hidden')
      this.buttonTarget.textContent = this.adjustTextValue
    }
  }
}