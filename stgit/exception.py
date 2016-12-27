class StgException(Exception):
    """Base class for all StGit exceptions."""


class StackException(StgException):
    """Exception raised by L{stack} objects."""
