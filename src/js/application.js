import { Application } from "@hotwired/stimulus"
import TimeBlockController from "./controllers/time_block_controller.js"

const application = Application.start()
application.register("time-block", TimeBlockController)

console.log("Stimulus application started")