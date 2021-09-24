archivo = '/home/pabs/CITIC/CongestionHospitalaria/datos/areas_concellos_correspondencia.csv'
archivo_esc = '/home/pabs/CITIC/CongestionHospitalaria/datos/areas_concellos_ao_correspondencia.csv'

with open(archivo, 'rt') as fichero:
    with open(archivo_esc, 'wt') as fichero_esc:
        for line in fichero:
            area, concello = line.strip().split(',')
            concello = concello.strip('"')
            area = area.strip('"')
            if 'A ' in concello:
                concello = concello.split('A ')[1]
                concello += ', A'
                print(concello)
            elif 'O ' in concello:
                concello = concello.split('O ')[1]
                concello += ', O'
                print(concello)
            elif 'As ' in concello:
                concello = concello.split('As ')[1]
                concello += ', As'
                print(concello)
            elif 'Os ' in concello:
                concello = concello.split('Os ')[1]
                concello += ', Os'
                print(concello)
            fichero_esc.write(f"{area};{concello}\n")