import { Application } from "@hotwired/stimulus"
import * as Turbo from "@hotwired/turbo"
import TimeBlockController from "./controllers/time_block_controller.js"
import CleaningWindowController from "./controllers/cleaning_window_controller.js"

// Start Turbo Drive
Turbo.start()

const application = Application.start()
application.register("time-block", TimeBlockController)
application.register("cleaning-window", CleaningWindowController)

console.log("Stimulus application started")
console.log("Turbo Drive started")