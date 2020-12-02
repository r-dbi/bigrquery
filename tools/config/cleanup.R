# Clean up files generated during configuration here.
# Use 'remove_file()' to remove files generated during configuration.

unlink(dir("./src", c(".o$", ".dll$"), recursive = TRUE, full.names = TRUE))
unlink("./src/google", recursive = TRUE)
unlink("./src/Makevars")
