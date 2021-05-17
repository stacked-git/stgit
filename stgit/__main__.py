if __name__ == '__main__':  # pragma: no cover
    import os
    import sys

    if os.environ.get('COVERAGE_PROCESS_START'):
        import coverage

        if len(sys.argv) < 2 or sys.argv[1].startswith('-'):
            context = 'stg'
        else:
            context = 'stg-' + sys.argv[1]

        cov = coverage.process_startup()
        cov.switch_context(context)

    from stgit.main import main

    main()
