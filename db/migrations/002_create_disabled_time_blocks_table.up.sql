CREATE TABLE IF NOT EXISTS disabled_time_blocks (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    apartment TEXT NOT NULL,
    date TEXT NOT NULL,
    time_of_day TEXT NOT NULL CHECK (time_of_day IN ('Morning', 'Afternoon')),
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(apartment, date, time_of_day)
);