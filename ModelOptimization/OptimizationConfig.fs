module Optimization

    type OptimizationConfig = { 
        LowerBound: double;
        UpperBound: double;
        Tolerance: double;
        Function: double -> double;
    }
    
    type OptimizationResult = {
        Extremum: double;
        X: double;
        IterationsCount: double;
    }


