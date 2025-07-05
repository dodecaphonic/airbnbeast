import { Controller } from "@hotwired/stimulus"

export default class extends Controller {
  static targets = ["grid", "button"]
  static values = { 
    adjustText: String,
    hideText: String
  }

  connect() {
    console.log("TimeBlock controller connected")
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

  toggleBlock(event) {
    const block = event.currentTarget
    const blockId = block.id
    
    if (block.classList.contains('bg-green-100')) {
      // Block this period
      block.classList.remove('bg-green-100', 'text-green-700', 'hover:bg-green-200')
      block.classList.add('bg-red-100', 'text-red-700', 'hover:bg-red-200', 'line-through')
    } else {
      // Unblock this period
      block.classList.remove('bg-red-100', 'text-red-700', 'hover:bg-red-200', 'line-through')
      block.classList.add('bg-green-100', 'text-green-700', 'hover:bg-green-200')
    }
    
    // TODO: Save state to backend/localStorage
    console.log('Toggled block:', blockId)
  }
}