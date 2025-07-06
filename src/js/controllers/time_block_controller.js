import { Controller } from "@hotwired/stimulus"

export default class extends Controller {
  static values = { 
    apartment: String,
    date: String,
    timeOfDay: String
  }

  connect() {
    console.log("TimeBlock controller connected")
  }

  async toggleBlock(event) {
    const block = event.currentTarget
    const isCurrentlyAvailable = block.classList.contains('bg-green-100')
    
    // Find the Turbo Frame that contains this time block
    const turboFrame = block.closest('turbo-frame')
    if (!turboFrame) {
      console.error('Could not find turbo-frame parent')
      return
    }
    
    try {
      const endpoint = isCurrentlyAvailable ? '/timeblocks/disable' : '/timeblocks/enable'
      const formData = new URLSearchParams({
        apartment: this.apartmentValue,
        date: this.dateValue,
        timeOfDay: this.timeOfDayValue
      })
      
      const response = await fetch(endpoint, {
        method: 'PATCH',
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded',
          'Turbo-Frame': turboFrame.id
        },
        body: formData.toString()
      })
      
      if (response.ok) {
        // Update the frame with the response content
        const html = await response.text()
        turboFrame.innerHTML = html
      } else {
        console.error('Failed to update time block:', response.status)
      }
    } catch (error) {
      console.error('Error updating time block:', error)
    }
  }
}