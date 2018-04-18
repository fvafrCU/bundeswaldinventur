fs <- dir()
for (f in fs) {
    if (f != "rename.R") {
        if (grepl("^test", f)) {
            n <- sub("^test_", "test-local_", f)
        } else {
            n <- sub("^t_", "test-bwibw_", f)
        }
        cmd <- paste("git mv", f, n)
        system(cmd)
    }
}
