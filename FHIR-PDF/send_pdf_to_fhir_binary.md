# Uploading a PDF to a FHIR Binary Endpoint via curl
# This example demonstrates how to manually send a PDF (e.g. rendered from a FHIR Questionnaire) to a FHIR `Binary` endpoint. This can be used as a migration path for integrating existing PDF documents into FHIR-based infrastructures like AKTIN.

## Endpoint
# The PDF is sent to the FHIR `Binary` endpoint, for example:
# In a production system, replace `localhost` with the hostname or IP address of the DWH server.

## curl Command

```bash
curl -X POST \
  http://localhost/aktin/cda/fhir/Binary \
  -H "Content-Type: application/pdf" \
  --data-binary "@example.pdf"
