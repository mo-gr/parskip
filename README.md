# parskip
parser/serializer for the eskip route definition format.

# Building

Use [Stack](http://docs.haskellstack.org/en/stable/README/) to build the tool

    stack build && stack install
    
This will install the `parskip` binary.

# Tests

Tests can be run with

    stack test

# Usage

Currently, `parskip` reads eskip from the standard in, parses it and serializes it pretty printed to standard out. Nothing else.

    parskip < someroutes.eskip
