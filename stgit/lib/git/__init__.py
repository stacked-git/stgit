"""A Python class hierarchy wrapping a git repository and its contents."""

from stgit.run import Run

from .branch import Branch
from .date import Date
from .iw import (
    CheckoutException,
    IndexAndWorktree,
    MergeConflictException,
    MergeException,
)
from .objects import Blob, BlobData, Commit, CommitData, Tree, TreeData
from .person import Person
from .repository import Repository, RepositoryException

__all__ = [
    'Blob',
    'BlobData',
    'Branch',
    'CheckoutException',
    'Commit',
    'CommitData',
    'Date',
    'IndexAndWorktree',
    'MergeConflictException',
    'MergeException',
    'Person',
    'Repository',
    'RepositoryException',
    'Tree',
    'TreeData',
]


def clone(remote, local):
    """Clone a remote repository using 'git clone'."""
    Run('git', 'clone', remote, local).run()
