#!/bin/bash
set -e

echo "Starting Airbnbeast application..."

# Run database migrations
echo "Running database migrations..."
node db/migrate.js up

# Start the application
echo "Starting main application..."
exec node -e "require('./output/Main/index.js').main()"