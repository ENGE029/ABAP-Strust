# ABAP-Strust AI Coding Instructions

## Project Overview
ABAP Trust Management utility for SAP systems. Manages SSL/TLS certificates in ABAP Trust Manager (STRUST) programmatically using ABAP 7.50+.

**Core Components:**
- `/apmg/cl_strust` - Main certificate management class (PSE operations, cert add/remove/update)
- `/apmg/cl_strust_cert_api` - Fetches certificates from external API (https://tools.abappm.com)
- `/apmg/strust_installer` - Interactive program for installing domain certificates
- `/apmg/strust_updater` - Batch program for updating expiring certificates
- `/apmg/strust_info` - Display program for listing certificates by type (own, root, intermediate, domain)
- `/apmg/strust_log` - Database table tracking all certificate changes

## ABAP-Specific Patterns

### Naming Conventions (abaplint enforced)
- **Namespace:** All objects prefixed with `/apmg/` or `/APMG/`
- **Variables:** Prefix with scope - `l_` (local), `g_` (global), `m_` (member), `<l_>` (field symbols)
- **Constants:** `c_` prefix (class constants) or `lc_` (local constants)
- **Method parameters:** `i_` (importing), `e_` (exporting), `c_` (changing), `r_` (returning)
- **Type definitions:** `ty_` prefix for structured types, `_tt` suffix for table types
  - Example: `ty_certattr`, `ty_certattr_tt`

### Error Handling Pattern
```abap
" Use custom exception class from dependency
RAISING /apmg/cx_error

" Always unlock PSE and cleanup on errors
IF sy-subrc <> 0.
  _unlock( ).
  RAISE EXCEPTION TYPE /apmg/cx_error_t100.  " For system errors
  " OR
  RAISE EXCEPTION TYPE /apmg/cx_error_text   " For custom messages
    EXPORTING text = 'Your message'(001).
ENDIF.
```

### Method Chaining for Fluent API
Methods return `VALUE(result) TYPE REF TO /apmg/cl_strust` for chainable operations:
```abap
strust->load( create = abap_true )->add_pem( pem )->update( comment ).
```

### PSE Lock/Unlock Pattern
CRITICAL: Always lock before operations, unlock after (even on errors):
```abap
_lock( ).
" ... PSE operations ...
_save( ).      " Calls _unlock internally
" OR on error
_unlock( ).    " Cleanup tempfile and dequeue
```

## Architecture & Data Flow

### Certificate Update Flow
1. **Load PSE** → `SSFPSE_LOAD` creates temp file, enqueues PSE
2. **Read current state** → `SSFC_GET_CERTIFICATELIST` / `SSFC_GET_OWNCERTIFICATE`
3. **Parse PEM** → Regex extract base64, `cl_abap_x509_certificate`, `SSFC_PARSE_CERTIFICATE`
4. **Modify certs** → `SSFC_PUT_CERTIFICATE` / `SSFC_REMOVECERTIFICATE`
5. **Save & notify** → `SSFPSE_STORE`, `ICM_SSL_PSE_CHANGED` (SSL contexts only)
6. **Log changes** → Insert to `/apmg/strust_log` with timestamp, user, comment

### External API Integration
`/apmg/cl_strust_cert_api` queries `https://tools.abappm.com/api/v1/certificates?domain=...`:
- Returns JSON with `peerCertificate` and `intermediateCertificates[]` arrays
- Wildcards (`*.example.com`) replaced with `api.example.com` for lookup
- Uses `ajson` dependency for JSON parsing (not native JSON in ABAP)
- HTTP client requires SSL ID (default `ANONYM`)

## Critical Development Workflows

### Adding New Certificate Methods
1. Update `/apmg/cl_strust` public section with method signature
2. Ensure `RAISING /apmg/cx_error` in signature
3. Call `_profile( )` to validate PSE is loaded
4. Wrap function modules with error handling: `_unlock( )` + raise on `sy-subrc <> 0`
5. Update `is_dirty = abap_true` if PSE modified

### Testing & Validation
- **abaplint:** All code must pass `.abaplint.json` rules before merge
  - Version: `v750` syntax only (no newer language features)
  - Enforces: object naming, method length, cyclomatic complexity, no public attributes
  - Use `##NO_TEXT` pragma to suppress text element warnings for constants
- **Authorization:** Programs check `cl_abap_pse=>authority_check( iv_activity = '01'|'02'|'06' )`
- **Test mode:** Programs support `p_test` checkbox - add certs without saving

### Working with Dependencies
Managed via `package.abap.json`:
- `ajson` (≥1.1.12) - JSON parsing for API responses
- `distinguished-name` (≥1.0.0) - DN parsing (`CN=`, `O=`, etc.)
- `error` (≥1.0.0) - Exception framework (`/apmg/cx_error`, `/apmg/cx_error_text`, `/apmg/cx_error_t100`)

Install via [apm](https://abappm.com) - do NOT vendor or inline dependencies.

## Special Considerations

### PSE Context/Application Mapping
Reference `c_context` and `c_application` constants - common combinations:
- `SSLC` + `ANONYM` = SSL Client Anonymous
- `SSLC` + `DFAULT` = SSL Client Standard
- `PROG` + `<SYST>` = System PSE

### Certificate Format Handling
- Input: PEM format with `-----BEGIN CERTIFICATE-----` / `-----END CERTIFICATE-----`
- Internal: xstring (binary DER format via `cl_abap_x509_certificate`)
- `add_pem( )` converts string→table, `add( )` processes table of 80-char lines

### Logging Pattern
All cert operations log to `/apmg/strust_log`:
```abap
_log_add(
  subject   = cert-subject
  issuer    = cert-issuer
  date_from = cert-date_from
  date_to   = cert-date_to
  status    = icon_led_green    " Use ICON_* constants
  message   = 'Added|Removed' ).
```
Batch insert via `_log_save( comment )` with timestamp.

### Report UI Patterns
- Use `SELECTION-SCREEN` blocks with `FRAME TITLE TEXT-tXX`
- Color output: `COLOR COL_POSITIVE` (green), `COL_NEGATIVE` (red), `COL_TOTAL` (yellow), `COL_GROUP` (orange)
- Formatting: Cert subject at 0-49, date_from at 130, date_to at 145, status at 158
- Certificate categorization: Root (self-signed: subject = issuer), Intermediate (subject ≠ issuer, no domain pattern), Domain (contains `.` in CN)

## Version & Licensing
- Version: `2.1.1` (update `c_version` constant when releasing)
- License: MIT (copyright 2025 apm.to Inc.)
- REUSE compliant: All files must have SPDX headers
