class Immutable:
    """Base class for immutable objects.

    Immutable objects cannot be modified once created. Any modification methods will
    return a new object, leaving the original object as it was.

    The reason for this is that we want to be able to represent git objects, which are
    immutable, and want to be able to create new git objects that are just slight
    modifications of other git objects. (Such as, for example, modifying the commit
    message of a commit object while leaving the rest of it intact. This involves
    creating a whole new commit object that's exactly like the old one except for the
    commit message.)

    The ``Immutable`` class does not actually enforce immutability--subclasses are
    responsible for enforcing immutability. Thus inheriting ``Immutable`` just serves as
    documentation.

    """
