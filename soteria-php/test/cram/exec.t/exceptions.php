<?php

function structured_loop()
{
    $count = 0;
    while ($count < 3) {
        while (true) {
            try {
                $count = $count + 1;
                echo "i";
                if ($count === 1) {
                    continue 2;
                }
                break 2;
            } finally {
                echo "f";
            }
        }
    }
    echo "done";
}

function caught_exception()
{
    try {
        throw new RuntimeException("boom");
    } catch (LogicException|DomainException) {
        echo "wrong";
    } catch (Exception $exception) {
        echo "caught";
        if ($exception === $exception) {
            echo ":same";
        }
        return 7;
    } finally {
        echo ":finally";
    }
}

function overridden_exception()
{
    try {
        throw 1;
    } finally {
        return 9;
    }
}

function caught_error()
{
    try {
        throw 1;
    } catch (Error) {
        echo ":error";
    }
}

structured_loop();
echo ":", caught_exception(), ":", overridden_exception();
caught_error();
