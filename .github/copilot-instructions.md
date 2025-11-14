# ABAP-Strust AI Coding Instructions

## Project Overview

ABAP module for programmatic certificate management in SAP Trust Management (STRUST). Provides API for adding, updating, and removing SSL/TLS certificates. Distributed via [apm](https://abappm.com) package manager.

**Core Components:**
- `/apmg/cl_strust` - Main API for certificate CRUD operations
- `/apmg/cl_strust_cert_api` - External API client to fetch certificates from tools.abappm.com
- `/apmg/strust_installer` - Interactive report for installing certificates by domain
- `/apmg/strust_updater` - Report for updating expiring certificates with renewal logic

## ABAP-Specific Conventions

### Naming Standards (enforced by abaplint)

**Namespace:** All objects use `/APMG/` prefix (package `/APMG/STRUST`)
- Classes: `/apmg/cl_strust*`
- Programs: `/apmg/strust_*`
- Tables: `/apmg/strust_*`
- Error namespace: `^(/APMG/|LCL_|TY_|LIF_)` for syntax checking

**Variable prefixes** (abaplint no_prefixes rule):
- Data: `^[LGM].?_` (local/global/member)
- Constants: `^[LGM]C_`
- Field-symbols: `^<[LGM].?_`
- Parameters: `^[ICER].?_` (importing/changing/exporting/returning)

### Code Style

- **Keywords:** UPPER CASE (enforced by abaplint)
- **Pretty printer:** Standard SAP formatting with indentation
- **Chaining:** Avoid chained assignments; use `;` for declarations when appropriate
- **Method chaining:** Fluent API pattern - most methods return `TYPE REF TO /apmg/cl_strust` for chaining
- **Exception handling:** All public methods raise `/apmg/cx_error` (from ABAP-Error dependency)

Example from `/apmg/cl_strust`:
```abap
strust->load( create = abap_true )->add_pem( certificate )->update( ).
```

### Critical PSE Operations Pattern

**Lock-Modify-Save-Unlock workflow** (see `_lock`, `_save`, `_unlock` methods):
1. Lock PSE context using `SSFPSE_CREATE_ENQUEUE`
2. Modify certificates in memory
3. Save to PSE using `SSFPSE_PUTCERTIFICATE_LIST`
4. Always unlock in cleanup, even on errors

**Context/Application pairs** define PSE locations (constants in `c_context` and `c_application`):
- SSLC/ANONYM: Anonymous SSL client (most common)
- SSLC/DFAULT: Default SSL client/server
- PROG/SYST: System PSE

## Dependencies & External APIs

**Required packages** (package.abap.json):
- `ajson` ^1.1.12 - JSON parsing (zbcgua/ajson)
- `distinguished-name` ^1.0.0 - Certificate DN parsing
- `error` ^1.0.0 - Exception base class

**External API & RFC Destination:**
- `/apmg/cl_strust_cert_api` queries certificate API via RFC destination `STRUST_API`
- Default endpoint: `https://tools.abappm.com/api/v1/certificates`
- RFC destination auto-created by abapGit from `src/strust_api.http.xml`
- Handles wildcard domains by replacing `*` with `api` subdomain
- Proxy configuration managed centrally in SM59 (no code changes needed)

## Version & Compatibility

- **Target:** SAP Basis 7.50+ (abaplint syntax version v750)
- **Version constant:** `c_version` in `/apmg/cl_strust` must match `package.abap.json` version
- **Linting:** All code checked against abaplint.json (682 lines) before merge

## Testing & Development Workflow

**Installation:**
- Via apm: `apm install strust` (global) or dependency in project
- Via abapGit: Clone `https://github.com/abapPM/ABAP-Strust` to package `/APMG/STRUST`

**Testing certificates:**
1. Run `/APMG/STRUST_INSTALLER` with domain (e.g., `*.example.com`)
2. Check installation in STRUST transaction
3. Test updates with `/APMG/STRUST_UPDATER` (filters by expiry days, default 30)

**Authorization required:** S_DEVELOP authority for PSE operations (activities 01, 02, 06)

## Key Files Reference

- `src/#apmg#cl_strust.clas.abap` - Main API (809 lines, fluent interface)
- `abaplint.json` - Comprehensive linting rules (avoid `db_operation_in_loop` exceptions, strict naming)
- `package.abap.json` - APM package manifest with semver
- `CONTRIBUTING.md` - Code of conduct, compatibility requirements (7.31+ for general Marc Bernard Tools)
