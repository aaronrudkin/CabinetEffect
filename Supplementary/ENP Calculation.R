enp = function(partyData)
{
    if(typeof(partyData)=="list")
    {
        results = numeric(length(partyData))
        j=1
        for(i in partyData)
        {
            results[j] = enp(i)
            j=j+1 
        }
        return(results)
    }
    else
    {
        return(1/sum((partyData/sum(partyData))^2))
    }
}

canadaParties = list(
                    c(118, 66, 28, 13, 20),
                    c(191, 41, 13, 10, 1, 6),
                    c(169, 51, 23, 15, 1, 6),
                    c(111, 104, 25, 19, 6),
                    c(208, 48, 8, 1),
                    c(116, 99, 30, 19, 1),
                    c(128, 95, 24, 17, 1),
                    c(131, 97, 21, 14, 2),
                    c(154, 72, 22, 14, 2),
                    c(109, 107, 31, 15, 2),
                    c(141, 95, 16, 11, 1),
                    c(136, 114, 26, 6, 0),
                    c(147, 103, 32, 0),
                    c(211, 40, 30, 1),
                    c(169, 83, 43, 0),
                    c(177, 54, 52, 9, 2, 1),
                    c(155, 60, 44, 21, 20, 1),
                    c(172, 66, 38, 13, 12),
                    c(135, 99, 54, 19, 1),
                    c(124, 103, 51, 29, 1),
                    c(143, 77, 49, 37, 2),
                    c(166, 103, 34, 4, 1),
                    c(184, 99, 44, 10, 1))

nzPreMPP = list(
                c(38, 42),
                c(46, 34),
                c(50, 30),
                c(45, 35),
                c(39, 41),
                c(46, 34),
                c(45, 35),
                c(44, 35, 1),
                c(45, 39),
                c(32, 55),
                c(55, 32),
                c(51, 40, 1),
                c(47, 43, 2),
                c(37, 56, 2),
                c(40, 57),
                c(67, 29, 1),
                c(50, 45, 4))

nzPostMPP = list(
                c(44, 37, 17, 13, 8, 1),
                c(49, 39, 10, 9, 7, 5),
                c(52, 27, 13, 9, 9, 8, 2),
                c(50, 48, 7, 6, 4, 3, 2, 1),
                c(58, 43, 9, 5, 5, 1, 1, 0),
                c(58, 34, 14, 8, 3, 1, 1, 1),
                c(60, 32, 14, 11, 2, 1, 1, 0))

enp(canadaParties)
enp(nzPreMPP)
