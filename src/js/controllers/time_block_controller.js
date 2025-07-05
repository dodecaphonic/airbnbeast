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
    
    // Update UI immediately for better UX
    if (isCurrentlyAvailable) {
      // Block this period
      block.classList.remove('bg-green-100', 'text-green-700', 'hover:bg-green-200')
      block.classList.add('bg-red-100', 'text-red-700', 'hover:bg-red-200', 'line-through')
    } else {
      // Unblock this period
      block.classList.remove('bg-red-100', 'text-red-700', 'hover:bg-red-200', 'line-through')
      block.classList.add('bg-green-100', 'text-green-700', 'hover:bg-green-200')
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
        },
        body: formData.toString()
      })
      
      if (response.redirected) {
        // Server responded with redirect, follow it
        window.location.href = response.url
      } else if (!response.ok) {
        // Request failed, revert UI changes
        console.error('Failed to update time block:', response.status)
        this.revertUIChanges(block, isCurrentlyAvailable)
      }
    } catch (error) {
      console.error('Error updating time block:', error)
      this.revertUIChanges(block, isCurrentlyAvailable)
    }
  }
  
  revertUIChanges(block, wasAvailable) {
    if (wasAvailable) {
      // Revert to available
      block.classList.remove('bg-red-100', 'text-red-700', 'hover:bg-red-200', 'line-through')
      block.classList.add('bg-green-100', 'text-green-700', 'hover:bg-green-200')
    } else {
      // Revert to blocked
      block.classList.remove('bg-green-100', 'text-green-700', 'hover:bg-green-200')
      block.classList.add('bg-red-100', 'text-red-700', 'hover:bg-red-200', 'line-through')
    }
  }
}