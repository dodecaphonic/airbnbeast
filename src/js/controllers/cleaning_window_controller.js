import { Controller } from "@hotwired/stimulus";

export default class extends Controller {
  static targets = ["grid", "button"];
  static values = {
    adjustText: String,
    hideText: String,
    timeBlocksVisible: { type: Boolean, default: false },
  };

  connect() {
    console.log("CleaningWindow controller connected");

    this.renderOpenState();
  }

  toggle() {
    this.timeBlocksVisibleValue = !this.timeBlocksVisibleValue;
    this.renderOpenState();
  }

  renderOpenState() {
    const grid = this.gridTarget;

    if (this.timeBlocksVisibleValue) {
      grid.classList.remove("hidden");
      this.buttonTarget.textContent = this.hideTextValue;
    } else {
      grid.classList.add("hidden");
      this.buttonTarget.textContent = this.adjustTextValue;
    }
  }
}
