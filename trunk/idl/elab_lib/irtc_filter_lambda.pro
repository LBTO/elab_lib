function irtc_filter_lambda, filter_number

    CASE filter_number OF
    	1: lambda = 0.			;BROADBAND
    	2: lambda = 1.07e-6	;J
    	3: lambda = 1.60e-6	;H
    ENDCASE

return, lambda

end