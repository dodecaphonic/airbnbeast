CREATE TABLE done_guest_stays (
  guest_stay_id TEXT PRIMARY KEY,
  marked_done_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (guest_stay_id) REFERENCES guest_stays(id) ON DELETE CASCADE
);

-- Add index for common queries
CREATE INDEX idx_done_guest_stays_guest_stay_id ON done_guest_stays(guest_stay_id);