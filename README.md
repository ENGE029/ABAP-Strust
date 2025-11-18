![Version](https://img.shields.io/endpoint?url=https://shield.abappm.com/github/abapPM/ABAP-Strust/src/%2523apmg%2523cl_strust.clas.abap/c_version&label=Version&color=blue)

[![License](https://img.shields.io/github/license/abapPM/ABAP-Strust?label=License&color=success)](LICENSE)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg?color=success)](https://github.com/abapPM/.github/blob/main/CODE_OF_CONDUCT.md)
[![REUSE Status](https://api.reuse.software/badge/github.com/abapPM/ABAP-Strust)](https://api.reuse.software/info/github.com/abapPM/ABAP-Strust)

# Trust Management

Easy-to-use class for adding, updating, or removing certificates from ABAP Trust Management (transaction STRUST)

NO WARRANTIES, [MIT License](LICENSE)

## Usage

### Install Certificates

Run program `/APMG/STRUST_INSTALLER` and enter the domain for which you want to install the required certificates to ABAP Trust Management:

![Installer Selection-Screen](https://github.com/abapPM/ABAP-Strust/raw/main/img/installer-1.png)

![Installer Result](https://github.com/abapPM/ABAP-Strust/raw/main/img/installer-2.png)

### Update Certificates

Run program `/APMG/STRUST_UPDATER` and optionally enter domains for which you want to update the certificates to ABAP Trust Management:

![Updater Selection-Screen](https://github.com/abapPM/ABAP-Strust/raw/main/img/updater-1.png)

![Updater Result](https://github.com/abapPM/ABAP-Strust/raw/main/img/updater-2.png)

![Updater Result](https://github.com/abapPM/ABAP-Strust/raw/main/img/updater-3.png)

![Updater Result with Root Intermediate Certificates](https://github.com/abapPM/ABAP-Strust/raw/main/img/updater-4.png)

### Cleanup Certificates

Run program `/APMG/STRUST_CLEANER` to remove expired and duplicate certificates from ABAP Trust Management:

![Cleaner Selection-Screen](https://github.com/abapPM/ABAP-Strust/raw/main/img/cleaner-1.png)

The cleaner identifies and removes:
- **Expired certificates**: Certificates past their validity date
- **Duplicate certificates**: Multiple certificates with the same subject/issuer (keeps the newest)
- Optional filters for root, intermediate, and domain certificates

## API

Example of creating, updating, or removing a certificate using class `/apmg/cl_strust`.

```abap
CONSTANTS:
  c_sslc    TYPE psecontext VALUE 'SSLC' ##NO_TEXT,
  c_anonym  TYPE ssfappl VALUE 'ANONYM' ##NO_TEXT,
  c_id      TYPE ssfid VALUE 'CN=%SID SSL client SSL Client (Standard), OU=%ORG, O=MBT, C=CA' ##NO_TEXT,
  c_org     TYPE string VALUE 'Marc Bernard Tools' ##NO_TEXT,
  c_subject TYPE string VALUE 'CN=*.marcbernardtools.com' ##NO_TEXT.

DATA(strust) = /apmg/cl_strust=>create(
  context     = c_sslc
  application = c_anonym ).

strust->load(
  create = abap_true
  id     = c_id
  org    = c_org ).

strust->get_own_certificate( ).

strust->get_certificate_list( ).

IF drop = abap_true.
  strust->remove( c_subject ).
ELSE.
  strust->add_pem( '<your_certificate>' ).
  DATA(result) = strust->update( ).
  " result-added contains newly added certificates
  " result-removed contains removed certificates (if remove_expired = abap_true)
ENDIF.
```

The certificate for the `add` method needs to be provided as a table with the following format:

```txt
-----BEGIN CERTIFICATE-----
MIIGQDCCBSigAwIBAgIQCNqWSvYNNa9hfOzsk89rUjANBgkqhkiG9w0BAQsFADBg
MQswCQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3
...
-----END CERTIFICATE-----
```

Alternatively, use the method `add_pem` to pass the certificate as a string.

## Prerequisites

- SAP Basis 7.50 or higher
- Packages:
  - [`ajson`](https://github.com/sbcgua/ajson)
  - [`error`](https://github.com/abapPM/ABAP-Error)
  - [`distinguished-name`](https://github.com/abapPM/ABAP-Distinguished-Name)

## Installation

Install `strust` as a global module in your system using [apm](https://abappm.com).

or

Specify the `strust` module as a dependency in your project and import it to your namespace using [apm](https://abappm.com).

### RFC Destination Setup

The installer and updater programs require an HTTP RFC destination named `STRUST_API` to fetch certificates from the external API.

**Create RFC destination in SM59:**

1. Go to transaction **SM59**
2. Create new RFC destination (type **G** - HTTP connection to external server)
3. **RFC Destination:** `STRUST_API`
4. **Description:** `STRUST Certificate API`
5. **Technical Settings:**
   - **Target Host:** `tools.abappm.com`
   - **Port:** `443`
   - **Path Prefix:** `/api/v1/certificates`
6. **Logon & Security:**
   - **SSL:** Active
   - **SSL Certificate:** `ANONYM` (or your SSL client PSE)
7. **Special Options:**
   - If you need proxy access, configure it in the **Proxy Settings** tab
8. **Test Connection** to verify

This setup allows centralized proxy configuration and secure certificate management without hardcoding URLs in the code.

## Contributions

All contributions are welcome! Read our [Contribution Guidelines](https://github.com/abapPM/ABAP-Strust/blob/main/CONTRIBUTING.md), fork this repo, and create a pull request.

You can install the developer version of ABAP STRUST using [abapGit](https://github.com/abapGit/abapGit) by creating a new online repository for `https://github.com/abapPM/ABAP-Strust`.

Recommended SAP package: `/APMG/STRUST`

## About

Made with ❤ in Canada

Copyright 2025 apm.to Inc. <https://apm.to>

Follow [@marcf.be](https://bsky.app/profile/marcf.be) on Bluesky and [@marcfbe](https://linkedin.com/in/marcfbe) or LinkedIn
