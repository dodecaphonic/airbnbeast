CREATE TABLE guest_stays (
  id TEXT PRIMARY KEY,
  apartment TEXT NOT NULL,
  from_date TEXT NOT NULL,
  to_date TEXT NOT NULL,
  last_4_digits TEXT NOT NULL,
  link TEXT NOT NULL,
  notes TEXT,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Add indexes for common queries
CREATE INDEX idx_guest_stays_apartment ON guest_stays(apartment);
CREATE INDEX idx_guest_stays_dates ON guest_stays(from_date, to_date);