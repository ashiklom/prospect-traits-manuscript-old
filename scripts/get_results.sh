#!/bin/bash
ssh geo 'echo -e "PRAGMA foreign_keys = on;\n.dump results" | sqlite3 ~/dietzelab/curated-leafspec/leaf_spectra.db' > results.sql
