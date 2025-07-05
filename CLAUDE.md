# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Airbnbeast is a PureScript application that manages Airbnb calendar data and generates cleaning schedules for rental properties. It fetches iCal calendar data from Airbnb, parses reservations and availability, and creates optimal cleaning windows between guest stays.

## Architecture

### Core Modules

- **Main.purs**: Entry point that fetches guest stays for two apartments (Gloria and Santa) and generates cleaning schedules
- **Airbnbeast.Availability**: Handles fetching and parsing of Airbnb calendar data
  - Fetches iCal data from Airbnb URLs
  - Converts calendar events to guest stays and availability data
  - Provides daily occupancy views
- **Airbnbeast.Cleaning**: Generates cleaning schedules from guest stay data
  - Creates cleaning windows between guest stays
  - Determines weekend coverage for scheduling
  - Provides pretty-printing for cleaning schedules
- **Airbnbeast.Parser**: Parses iCal events into structured data
  - Handles "Reserved" events with guest information
  - Handles "Airbnb (Not available)" events
  - Extracts reservation URLs and last 4 digits from descriptions
- **Node.ICal**: Foreign function interface for iCal parsing
  - Wraps node-ical JavaScript library
  - Provides type-safe parsing of iCal data

### Data Flow

1. Main fetches guest stays for configured apartments
2. Availability module fetches iCal data from Airbnb URLs
3. Parser converts iCal events to structured reservation/availability data
4. Cleaning module generates cleaning windows between guest stays
5. Results are pretty-printed to console

## Development Commands

### Building and Running
```bash
# Build the project
spago build

# Run the main application
spago run

# Run in development mode with file watching
spago run --watch
```

### Testing
```bash
# Run all tests
spago test

# Run tests with file watching
spago test --watch
```

### Code Quality
```bash
# Format code
purs-tidy format-in-place 'src/**/*.purs'
purs-tidy format-in-place 'test/**/*.purs'

# Check formatting
purs-tidy check 'src/**/*.purs'
purs-tidy check 'test/**/*.purs'
```

## Development Environment

The project uses Nix for dependency management. Available tools:
- PureScript compiler (v0.15.4)
- Spago build tool
- purs-tidy for formatting
- Node.js 16.x for JavaScript runtime

To enter the development environment:
```bash
nix-shell
# or with flakes
nix develop
```

## Key Dependencies

- **PureScript Standard Library**: Core functional programming utilities
- **node-ical**: JavaScript library for parsing iCal data
- **aff**: Asynchronous effects
- **datetime**: Date and time handling
- **profunctor-lenses**: Functional lenses for data manipulation
- **spec**: Testing framework

## File Structure

```
src/
├── Main.purs                 # Application entry point
├── Airbnbeast/
│   ├── Availability.purs     # Calendar fetching and parsing
│   ├── Cleaning.purs         # Cleaning schedule generation
│   └── Parser.purs           # iCal event parsing
└── Node/
    ├── ICal.purs            # PureScript FFI bindings
    └── ICal.js              # JavaScript FFI implementation

test/
└── Main.purs                # Test suite
```

## Configuration

The application is configured with hardcoded Airbnb calendar URLs in Main.purs. To add new apartments or change URLs, modify the apartment definitions in the main function.

## Testing Strategy

Tests cover:
- iCal parsing functionality
- Event parsing for reservations and unavailability
- Cleaning window generation logic
- Weekend coverage calculation