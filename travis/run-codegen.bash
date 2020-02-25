#!/bin/bash
set -ex -o pipefail
export PERL_MM_USE_DEFAULT=1
cpan install XML::LibXML XML::LibXSLT
perl /xml/codegen.pl | tee /out.txt
