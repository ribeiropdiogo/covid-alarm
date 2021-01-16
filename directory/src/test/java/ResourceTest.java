import common.District;
import directory.Data;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;

public class ResourceTest {
    private static List<District> infoPerDistrict = new ArrayList<>();
    private static final double DELTA = 1e-15;

    static {
        Map<String, Integer> locationsBraga = new HashMap<String, Integer>();
        locationsBraga.put("Guimaraes", 100);
        locationsBraga.put("Braga", 160);
        locationsBraga.put("Vizela", 17);
        locationsBraga.put("VilaVerde", 30);
        locationsBraga.put("Barcelos", 34);
        locationsBraga.put("PovoaLanhoso", 34);

        Map<String, Integer> locationsLisboa = new HashMap<String, Integer>();
        locationsLisboa.put("Oeiras", 115);
        locationsLisboa.put("Lisboa", 400);
        locationsLisboa.put("Cascais", 200);
        locationsLisboa.put("Loures", 300);
        locationsLisboa.put("Amadora", 45);
        locationsLisboa.put("Odivelas", 45);
        locationsLisboa.put("Sintra", 65);
        locationsLisboa.put("Mafra", 80);

        Map<String, Integer> locationsPorto = new HashMap<String, Integer>();
        locationsPorto.put("Porto", 350);
        locationsPorto.put("StoTirso", 10);
        locationsPorto.put("Gaia", 17);
        locationsPorto.put("Maia", 30);
        locationsPorto.put("Matosinhos", 45);
        locationsPorto.put("Gondomar", 45);

        Map<String, Integer> locationsAveiro = new HashMap<String, Integer>();
        locationsAveiro.put("Agueda", 150);
        locationsAveiro.put("Ovar", 100);
        locationsAveiro.put("Espinho", 127);
        locationsAveiro.put("Aveiro", 90);
        locationsAveiro.put("Estarreja", 45);

        Map<String, Integer> locationsLeiria = new HashMap<String, Integer>();
        locationsLeiria.put("Leiria", 90);
        locationsLeiria.put("Ansiao", 100);
        locationsLeiria.put("Batalha", 127);
        locationsLeiria.put("Alcobaca", 90);
        locationsLeiria.put("Peniche", 45);
        locationsLeiria.put("Obidos", 30);

        Map<String, Integer> locationsCoimbra = new HashMap<String, Integer>();
        locationsCoimbra.put("Coimbra", 75);
        locationsCoimbra.put("Penacova", 50);
        locationsCoimbra.put("Mira", 17);
        locationsCoimbra.put("Penela", 29);
        locationsCoimbra.put("Soure", 35);

        District d1 = new District("Lisboa", 1000, 500, 100, locationsLisboa);
        District d3 = new District("Porto", 1000, 400, 100, locationsPorto);
        District d4 = new District("Aveiro", 1000, 350, 100, locationsAveiro);
        District d5 = new District("Leiria", 1000, 450, 200, locationsLeiria);
        District d6 = new District("Coimbra", 1000, 200, 200, locationsCoimbra);
        District d2 = new District("Braga", 1000, 300, 200, locationsBraga);

        infoPerDistrict.add(d1);
        infoPerDistrict.add(d2);
        infoPerDistrict.add(d3);
        infoPerDistrict.add(d4);
        infoPerDistrict.add(d5);
        infoPerDistrict.add(d6);
    }

    @Test
    public void getRacioMostInfectedTest() {
        Map<String, Float> res = Data.getRacioMostInfected(infoPerDistrict);
        Map<String, Float> expect = new HashMap<>();

        expect.put("Lisboa", 0.5f);
        expect.put("Leiria", 0.45f);
        expect.put("Porto", 0.4f);
        expect.put("Aveiro", 0.35f);
        expect.put("Braga", 0.3f);

        // assert statements
        assertEquals("Os distritos com maior rácio de infectados são:", expect, res);
    }

    @Test
    public void getTopCrowdedTest() {
        List<String> res = Data.top5CrowdedLocation(infoPerDistrict);
        List<String> expect = new ArrayList<>();

        expect.add("Lisboa");
        expect.add("Porto");
        expect.add("Loures");
        expect.add("Cascais");
        expect.add("Braga");

        // assert statements
        assertEquals("Os locais com mais pessoas simultaneamente são:", expect, res);
    }

    @Test
    public void meanMeetInfectedTest() {
        float res = Data.meanMeetInfected(infoPerDistrict);

        float expect = 0.15f;

        //número médio de utilizadores que se cruzaram com utilizadores declarados doentes.
        //totalMeetInfected/totalUsers <=> 900/6000 = 0.15
        assertEquals(expect, res, DELTA);
    }

    @Test
    public void getPerDistrictTest() {
        Map<String, Integer> locationsLisboa = new HashMap<String, Integer>();
        locationsLisboa.put("Oeiras", 115);
        locationsLisboa.put("Lisboa", 400);
        locationsLisboa.put("Cascais", 200);
        locationsLisboa.put("Loures", 300);
        locationsLisboa.put("Amadora", 45);
        locationsLisboa.put("Odivelas", 45);
        locationsLisboa.put("Sintra", 65);
        locationsLisboa.put("Mafra", 80);

        District d1 = new District("Lisboa", 10000, 500, 100, locationsLisboa);

        Data.addDistrict(d1);
        int res1 = Data.getInfectedPerDistrict("Lisboa");
        int expect1 = 500;

        int res2 = Data.getUsersPerDistrict("Lisboa");
        int expect2 = 10000;


        // assert statements
        assertEquals("Número de infectados em Lisboa:", expect1, res1);
        assertEquals("Número total de users em Lisboa: ", expect2, res2);
    }

    @Test
    public void removeDistrictTest() {
        Map<String, Integer> locationsLisboa = new HashMap<String, Integer>();
        locationsLisboa.put("Oeiras", 115);
        locationsLisboa.put("Lisboa", 400);
        locationsLisboa.put("Cascais", 200);
        locationsLisboa.put("Loures", 300);
        locationsLisboa.put("Amadora", 45);
        locationsLisboa.put("Odivelas", 45);
        locationsLisboa.put("Sintra", 65);
        locationsLisboa.put("Mafra", 80);

        District d1 = new District("Lisboa", 10000, 500, 100, locationsLisboa);

        District d2 = new District("Porto", 10000, 500, 100, locationsLisboa);


        Data.addDistrict(d1);
        Data.addDistrict(d2);


        boolean actual = Data.removeDistrict("Lisboa");

        boolean actualPorto = Data.removeDistrict("Porto");


        // assert statements
        assertEquals("Remover o distrito de Lisboa:",true, actual);
        assertEquals("Remover o distrito do Porto:", true, actualPorto);
    }
}
