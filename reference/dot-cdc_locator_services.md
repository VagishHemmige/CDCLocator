# Valid service enums for the CDC Locator API

These values are passed to the `services` query parameter of the
`/providers` endpoint as a comma-separated list.

## Usage

``` r
.cdc_locator_services
```

## Format

An object of class `character` of length 14.

## Details

The following enums are supported:

- hivtesting : HIV Testing

- stdtesting : STD Testing

- selftesting : Self-administered HIV & STD testing

- preptesting : Pre-Exposure Prophylaxis (PrEP)

- pep : Post-Exposure Prophylaxis (PEP)

- condom : Condom distribution

- Hepc : Hepatitis C testing

- doxypep : Doxycycline post-exposure prophylaxis

- mailintesting : Mail-in HIV self-testing kits

- ryanwhite : Ryan White HIV/AIDS Program services

- clinics : HRSA-funded health clinics

- substanceabuse : Substance use disorder treatment

- mentalhealth : Mental health services

- housing : Housing assistance
