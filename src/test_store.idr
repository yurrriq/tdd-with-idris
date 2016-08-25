import DataStore

test_store : DataStore (SString .+. SString .+. SInt)
test_store = addToStore ("Mercury", "Mariner 10",   1974)
           $ addToStore ("Venus",   "Venera",       1961)
           $ addToStore ("Uranus",  "Voyager 2",    1986)
           $ addToStore ("Pluto",   "New Horizons", 2015)
           $ empty
