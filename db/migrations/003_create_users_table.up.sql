CREATE TABLE users (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT NOT NULL UNIQUE,
  password_hash TEXT NOT NULL,
  is_admin BOOLEAN NOT NULL DEFAULT 0,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Create default admin user (password will need to be set manually)
-- Default password: "admin123" (hashed)
INSERT INTO users (username, password_hash, is_admin) VALUES 
  ('vitor', '$2b$10$rN8JSLGzEkAYfkV6X9r3E.XK4j9WM5JZKhRhqOGVR1M2b8JN7FPnm', 1);

-- Create default regular user (password will need to be set manually)  
-- Default password: "user123" (hashed)
INSERT INTO users (username, password_hash, is_admin) VALUES
  ('sandra', '$2b$10$8kE4F3V5c0Qr8yN1X2a7z.YJ5k8LmBGEeRhNtOkVP3L4b9JM6CPer', 0);