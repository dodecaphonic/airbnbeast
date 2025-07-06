# Multi-stage Docker build for Airbnbeast PureScript application
FROM node:lts-slim AS builder

# Install system dependencies needed for building
RUN apt-get update && apt-get install -y \
    build-essential \
    python3 \
    libsqlite3-dev \
    sqlite3 \
    git \
    wget \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy package files first for better caching
COPY package*.json ./
COPY spago.* ./

# Install all dependencies (including dev dependencies for building)
RUN npm ci

# Install PureScript and build tools
RUN npm install -g purescript@0.15.4 spago@next

# Copy source code
COPY src/ ./src/

# Copy the Tailwind config so it picks up the source files
COPY tailwind.config.js ./

# Build the project (PureScript, Tailwind, JavaScript)
RUN npm run build

# Production stage
FROM node:lts-slim AS production

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    sqlite3 \
    libsqlite3-0 \
    dumb-init \
    && rm -rf /var/lib/apt/lists/*

# Create app user for security
RUN groupadd --gid 1001 nodejs && \
    useradd --uid 1001 --gid nodejs --shell /bin/bash --create-home purescript

# Set working directory
WORKDIR /app

# Copy package files
COPY package*.json ./

# Install only production dependencies
RUN npm ci --only=production && npm cache clean --force

# Copy built application from builder stage
COPY --from=builder /app/output ./output
COPY --from=builder /app/dist ./dist

# Copy database migrations
COPY db/ ./db/

# Create directories for data and logs
RUN mkdir -p /app/data /app/logs && \
    chown -R purescript:nodejs /app

# Switch to non-root user
USER purescript

# Expose port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD node -e "const http = require('http'); \
    const options = { hostname: 'localhost', port: 8080, path: '/login', timeout: 2000 }; \
    const req = http.request(options, (res) => { \
    process.exit(res.statusCode === 200 ? 0 : 1); \
    }); \
    req.on('error', () => process.exit(1)); \
    req.on('timeout', () => process.exit(1)); \
    req.end();"

# Use dumb-init to handle signals properly
ENTRYPOINT ["/usr/bin/dumb-init", "--"]

# Start the application
CMD ["node", "-e", "require('./output/Main/index.js').main()"]