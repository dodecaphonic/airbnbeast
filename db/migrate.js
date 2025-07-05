#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const sqlite3 = require('sqlite3').verbose();

// Configuration
const DATABASE_PATH = process.env.DATABASE_PATH || 'db/airbnbeast.sqlite3';
const MIGRATIONS_DIR = 'db/migrations';

// Ensure database directory exists
const dbDir = path.dirname(DATABASE_PATH);
if (!fs.existsSync(dbDir)) {
    fs.mkdirSync(dbDir, { recursive: true });
}

// Database connection
const db = new sqlite3.Database(DATABASE_PATH);

// Helper functions
function parseFilename(filename) {
    const match = filename.match(/^(\d+)_(.+)\.(up|down)\.sql$/);
    if (!match) return null;
    
    return {
        number: parseInt(match[1], 10),
        description: match[2],
        direction: match[3],
        filename
    };
}

function getAvailableMigrations() {
    const files = fs.readdirSync(MIGRATIONS_DIR);
    const migrations = {};
    
    files.forEach(file => {
        const parsed = parseFilename(file);
        if (parsed) {
            if (!migrations[parsed.number]) {
                migrations[parsed.number] = {};
            }
            migrations[parsed.number][parsed.direction] = {
                ...parsed,
                path: path.join(MIGRATIONS_DIR, file)
            };
        }
    });
    
    return migrations;
}

function getAppliedMigrations() {
    return new Promise((resolve, reject) => {
        db.all("SELECT id FROM migrations ORDER BY id", (err, rows) => {
            if (err) {
                // If table doesn't exist, return empty array
                if (err.message.includes('no such table')) {
                    resolve([]);
                } else {
                    reject(err);
                }
            } else {
                resolve(rows.map(row => row.id));
            }
        });
    });
}

function executeSqlFile(filePath) {
    return new Promise((resolve, reject) => {
        const sql = fs.readFileSync(filePath, 'utf8').trim();
        if (!sql) {
            resolve();
            return;
        }
        
        db.exec(sql, (err) => {
            if (err) reject(err);
            else resolve();
        });
    });
}

function recordMigration(migrationId) {
    return new Promise((resolve, reject) => {
        db.run("INSERT INTO migrations (id) VALUES (?)", [migrationId], (err) => {
            if (err) reject(err);
            else resolve();
        });
    });
}

function removeMigrationRecord(migrationId) {
    return new Promise((resolve, reject) => {
        db.run("DELETE FROM migrations WHERE id = ?", [migrationId], (err) => {
            if (err) reject(err);
            else resolve();
        });
    });
}

async function migrateUp() {
    try {
        const availableMigrations = getAvailableMigrations();
        const appliedMigrations = await getAppliedMigrations();
        
        const migrationNumbers = Object.keys(availableMigrations)
            .map(n => parseInt(n, 10))
            .sort((a, b) => a - b);
        
        const pendingMigrations = migrationNumbers.filter(n => !appliedMigrations.includes(n));
        
        if (pendingMigrations.length === 0) {
            console.log('No pending migrations.');
            return;
        }
        
        console.log(`Found ${pendingMigrations.length} pending migration(s).`);
        
        for (const migrationId of pendingMigrations) {
            const migration = availableMigrations[migrationId];
            
            if (!migration.up) {
                console.error(`Missing up migration for ${migrationId}`);
                continue;
            }
            
            console.log(`Applying migration ${migrationId}: ${migration.up.description}`);
            
            try {
                await executeSqlFile(migration.up.path);
                await recordMigration(migrationId);
                console.log(`✓ Migration ${migrationId} applied successfully.`);
            } catch (err) {
                console.error(`✗ Failed to apply migration ${migrationId}:`, err.message);
                throw err;
            }
        }
        
        console.log('All migrations applied successfully.');
    } catch (err) {
        console.error('Migration failed:', err.message);
        process.exit(1);
    }
}

async function migrateDown() {
    try {
        const availableMigrations = getAvailableMigrations();
        const appliedMigrations = await getAppliedMigrations();
        
        if (appliedMigrations.length === 0) {
            console.log('No migrations to rollback.');
            return;
        }
        
        // Get the last applied migration
        const lastMigrationId = Math.max(...appliedMigrations);
        const migration = availableMigrations[lastMigrationId];
        
        if (!migration || !migration.down) {
            console.error(`Missing down migration for ${lastMigrationId}`);
            return;
        }
        
        console.log(`Rolling back migration ${lastMigrationId}: ${migration.down.description}`);
        
        try {
            // Remove from tracking first, then run the down migration
            await removeMigrationRecord(lastMigrationId);
            await executeSqlFile(migration.down.path);
            console.log(`✓ Migration ${lastMigrationId} rolled back successfully.`);
        } catch (err) {
            console.error(`✗ Failed to rollback migration ${lastMigrationId}:`, err.message);
            throw err;
        }
    } catch (err) {
        console.error('Rollback failed:', err.message);
        process.exit(1);
    }
}

async function showStatus() {
    try {
        const availableMigrations = getAvailableMigrations();
        const appliedMigrations = await getAppliedMigrations();
        
        const migrationNumbers = Object.keys(availableMigrations)
            .map(n => parseInt(n, 10))
            .sort((a, b) => a - b);
        
        console.log('Migration Status:');
        console.log('================');
        
        if (migrationNumbers.length === 0) {
            console.log('No migrations found.');
            return;
        }
        
        migrationNumbers.forEach(migrationId => {
            const migration = availableMigrations[migrationId];
            const isApplied = appliedMigrations.includes(migrationId);
            const status = isApplied ? '✓ Applied' : '✗ Pending';
            const description = migration.up ? migration.up.description : migration.down.description;
            
            console.log(`${migrationId.toString().padStart(3, '0')}: ${status.padEnd(10)} ${description}`);
        });
        
        const pendingCount = migrationNumbers.filter(n => !appliedMigrations.includes(n)).length;
        console.log(`\nTotal: ${migrationNumbers.length} migrations, ${pendingCount} pending`);
    } catch (err) {
        console.error('Failed to get status:', err.message);
        process.exit(1);
    }
}

function showHelp() {
    console.log(`
Migration Tool Usage:
    
    node db/migrate.js up      - Apply all pending migrations
    node db/migrate.js down    - Rollback the last migration
    node db/migrate.js status  - Show migration status
    node db/migrate.js help    - Show this help message

Environment Variables:
    DATABASE_PATH    - Path to SQLite database (default: db/airbnbeast.sqlite3)

Migration File Format:
    <number>_<description>.<up|down>.sql
    
    Examples:
        001_create_users_table.up.sql
        001_create_users_table.down.sql
        002_add_email_index.up.sql
        002_add_email_index.down.sql
`);
}

// Main execution
async function main() {
    const command = process.argv[2];
    
    switch (command) {
        case 'up':
            await migrateUp();
            break;
        case 'down':
            await migrateDown();
            break;
        case 'status':
            await showStatus();
            break;
        case 'help':
        case '--help':
        case '-h':
            showHelp();
            break;
        default:
            console.error('Unknown command. Use "help" for usage information.');
            process.exit(1);
    }
    
    db.close();
}

// Handle uncaught errors
process.on('unhandledRejection', (err) => {
    console.error('Unhandled promise rejection:', err.message);
    db.close();
    process.exit(1);
});

// Run if called directly
if (require.main === module) {
    main();
}