function scaleSR, SR, from_lambda, to_lambda
    return, exp(-( -(from_lambda/to_lambda)^2*alog(SR)))
end
