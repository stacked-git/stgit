class ObjectCache:
    """Cache to ensure only one Python object is created per Git object.

    This reduces memory consumption and makes object comparison very cheap.

    """

    def __init__(self, create):
        self.__objects = {}
        self.__create = create

    def __getitem__(self, name):
        if name not in self.__objects:
            self.__objects[name] = self.__create(name)
        return self.__objects[name]

    def __contains__(self, name):
        return name in self.__objects

    def __setitem__(self, name, val):
        assert name not in self.__objects
        self.__objects[name] = val
