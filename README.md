# parskip
parser/serializer for the eskip route definition format.

***Parskip is not yet hardened or battletested!***


# Building

Use [Stack](http://docs.haskellstack.org/en/stable/README/) to build the tool

    stack build && stack install
    
This will install the `parskip` binary.
    
# Usage

Currently, `parskip` reads eskip from the standard in, parses it and serializes it pretty printed to standard out. Nothing else.

    parskip < someroutes.eskip
