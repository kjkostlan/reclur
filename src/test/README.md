Repl tests should be copied into the relevant clj file.

Auto: These tests either work or they don't. Test them all before committing with (test.auto.summary/report-broken).

Demos: These tests either require human aesthetic judgment and/or provide a springboard for writing custom GUI boxes, graphics, etc. They should be tested from time to time, or if the commit is likely to break them.
