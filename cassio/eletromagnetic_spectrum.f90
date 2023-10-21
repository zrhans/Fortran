program eletromagnetic_spectrum

implicit none

real :: length

real :: c = 299792458 !velocity of light in meters for seconds

real :: h = 6.62607004*10**-34 !planck's constant in joules seconds

real :: f !frequency in hertz

real :: E1 !energy in joules

real :: E2 !energy in eletronvolts


print*, "set a value for the wavelength in meters"

read*, length


select case (length)

    case (0.000000001*10**-6:0.0000001*10**-6)

    print*, "cosmic rays"

    case (0.0000001*10**-6:0.00001*10**-6)

    print*, "gamma rays"

    case (0.00001*10**-6:0.01*10**-6)

    print*, "x rays"

    case (0.01*10**-6:0.100*10**-6)

    print*, "extreme ultraviolet"

    case (0.100*10**-6:0.280*10**-6)

    print*, "UVC (ultraviolet)"

    case (0.280*10**-6:0.315*10**-6)

    print*, "UVB (ultraviolet)"

    case (0.315*10**-6:0.400*10**6)

    print*, "UVA (ultraviolet)"

    case (0.400*10**-6:0.450*10**-6)

    print*, "violet color (visible light)"

    case (0.450*10**-6:0.495*10**-6)

    print*, "blue color (visible light)"

    case (0.495*10**-6:0.570*10**-6)

    print*, "green color (visible light)"

    case (0.570*10**-6:0.590*10**-6)

    print*, "yellow color (visible light)"

    case (0.590*10**-6:0.620*10**-6)

    print*, "orange color (visible light)"

    case (0.620*10**-6:0.750*10**-6)

    print*, "red color (visible light)"
    
    case (0.750*10**-6:1000*10**-6)

    print*, "infrared"

    case (1000*10**-6:1000000*10**-6)

    print*, "microwaves"

    case (1000000*10**-6:10000000000*10**-6)

    print*, "radio waves"

    case (10000000000*10**-6:10000000000000*10**-6)

    print*, "a m"

    case default

    print*, "invalid value of the wavelength"

end select


print*, "the wavelength is", length


f = c/length

print*,"the frequency of vibration is", f, "hertz"


E1 = h*f

E2 = h*f/(1.60217733*10**-19)

print*, "the energy of radiation is", E1, "joules", "or", E2, "eletronvolts"


read*,


end program eletromagnetic_spectrum