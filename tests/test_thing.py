from py_interpreter.monkey_thing import Boolean, Integer, String


def test_string_hash_key():
    hello1 = String(value="Hello World")
    hello2 = String(value="Hello World")
    diff1 = String(value="My name is johnny")
    diff2 = String(value="My name is johnny")

    assert hello1.hash_key() == hello2.hash_key()

    assert diff1.hash_key() == diff2.hash_key()

    assert hello1.hash_key() != diff1.hash_key()


def test_integer_hash_key():
    int1 = Integer(value=1)
    int2 = Integer(value=1)
    diff1 = Integer(value=2)
    diff2 = Integer(value=2)

    assert int1.hash_key() == int2.hash_key()

    assert diff1.hash_key() == diff2.hash_key()

    assert int1.hash_key() != diff1.hash_key()


def test_boolean_hash_key():
    bool1 = Boolean(value=True)
    bool2 = Boolean(value=True)
    diff1 = Boolean(value=False)
    diff2 = Boolean(value=False)

    assert bool1.hash_key() == bool2.hash_key()

    assert diff1.hash_key() == diff2.hash_key()

    assert bool1.hash_key() != diff1.hash_key()
