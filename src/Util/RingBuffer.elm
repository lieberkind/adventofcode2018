module Util.RingBuffer exposing
    ( RingBuffer
    , current
    , fromLists
    , roll
    )


type alias RingBuffer a =
    { previous : List a
    , current : a
    , next : List a
    }


fromLists : List a -> a -> List a -> RingBuffer a
fromLists prev curr next =
    { previous = prev
    , current = curr
    , next = next
    }


current : RingBuffer a -> a
current buffer =
    buffer.current


roll : RingBuffer a -> RingBuffer a
roll buffer =
    case buffer.next of
        x :: xs ->
            { previous = buffer.previous ++ [ buffer.current ]
            , current = x
            , next = xs
            }

        [] ->
            case buffer.previous of
                [] ->
                    { previous = []
                    , current = buffer.current
                    , next = []
                    }

                x :: xs ->
                    { previous = [ buffer.current ]
                    , current = x
                    , next = xs
                    }
