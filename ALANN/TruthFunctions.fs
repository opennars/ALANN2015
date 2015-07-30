module TruthFunctions

    open System
    open Types
    open Parameters

    //personality factor 
    let k = Parameters.K
    
    // Extended boolean operators
    let inline _and lst = lst |> List.fold (fun acc x -> acc * x) 1.0f
    let inline _not a = 1.0f - a
    let inline _or lst = 1.0f - (lst |> List.fold (fun acc x -> (1.0f - x) * acc) 1.0f)
    
    // Evidence conversion functions
    let inline w2c (wplus, w) = (wplus / w, w /(w + k))
    let inline c2w (f, c) = (k * f * c / (1.0f - c), k * c / (1.0f - c))   
    
    // Local inference
    let inline f_rev ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let a = c2w(f1, c1)
        let b = c2w(f2, c2)
        let wplus = (fst a + fst b)
        let w = (snd a + snd b)
        let f, c = w2c(wplus, w) 
        {F = f; C = c}

    let inline f_exp ({F = f; C = c}) = c * (f - 0.5f) + 0.5f

    let inline f_dec (p, d) = ( p * (d -0.5f))

    // Immediate inference
    let inline f_neg {F=f; C=c} = {F = 1.0f - f; C = c}
    let inline f_cnv {F=f; C=c} = {F = 1.0f; C = f * c / (f * c + k)} // w2c(_and [f; c], _and [f; c]) // wminus = 0 so w = wplus
    let inline f_cnt {F=f; C=c} = 
        let f, c = w2c(0.0f, _and [_not f; c ])
        {F = f; C = c} // wplus = 0 so w = wminus
    
    // Strong syllogism
    let inline f_ded ({F=f1; C=c1}, {F=f2; C=c2}) = {F = _and[f1; f2]; C = _and[f1; f2; c1; c2]}
    let inline f_ana ({F=f1; C=c1}, {F=f2; C=c2}) = {F = _and[f1; f2]; C = _and[f2; c1; c2]}
    let inline f_res  ({F=f1; C=c1}, {F=f2; C=c2}) = {F = _and[f1; f2]; C = _and[_or [f1; f2]; c1; c2]}
    
    // Weak syllogism    
    let inline f_abd ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let f, c = w2c( _and [f1; f2; c1; c2], _and [f1; c1; c2])
        {F = f; C = c}
    let inline f_ind ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let f,c = w2c( _and [f1; f2; c1; c2], _and [f2; c1; c2])
        {F = f; C = c}
    let inline f_exe ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let f, c = w2c( _and [f1; f2; c1; c2], _and [f1; f2; c1; c2])
        {F = f; C = c}
    let inline f_com ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let f, c = w2c( _and [f1; f2; c1; c2], _and [_or [f1; f2]; c1; c2])
        {F = f; C = c}
    
    // Term composition
    let inline f_int ({F=f1; C=c1}, {F=f2; C=c2}) = {F = _and [f1; f2]; C = _and [c1; c2]}
    let inline f_uni ({F=f1; C=c1}, {F=f2; C=c2}) = {F = _or [f1; f2]; C = _and [c1; c2]}
    let inline f_dif ({F=f1; C=c1}, {F=f2; C=c2}) = {F = _and [f1; _not f2]; C = _and [c1; c2]}
    
    // Term decomposition
    let inline f_pnn ({F=f1; C=c1}, {F=f2; C=c2}) =  
        let f2n = _not f2
        let fn = _and [f1; f2n] 
        let f = _not fn
        let c = _and [fn; c1; c2]
        {F = f; C = c}

    let inline f_npp ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let f1n = _not f1
        let f = _and [f1n; f2]
        let c = _and [f; c1; c2]
        {F = f; C = c}

    let inline f_pnp ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let f2n = _not f2
        let f = _and [f1; f2n]
        let c = _and [f; c1; c2]
        {F = f; C = c}

    let inline f_nnn ({F=f1; C=c1}, {F=f2; C=c2}) = 
        let f1n = _not f1
        let f2n = _not f2
        let fn = _and [f1n; f2n]
        let f = _not fn
        let c = _and [fn; c1; c2]
        {F = f; C = c}
