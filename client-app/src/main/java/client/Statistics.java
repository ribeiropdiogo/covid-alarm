package client;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.*;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.Iterator;

public class Statistics {

  private static final String baseURL = "http://localhost:8080";

  private static String districtNameEncoder(String district) {
    switch (district) {
      case "Aveiro":
        return "Aveiro";
      case "Beja":
        return "Beja";
      case "Braga":
        return "Braga";
      case "Bragança":
        return "Braganca";
      case "Castelo Branco":
        return "CasteloBranco";
      case "Coimbra":
        return "Coimbra";
      case "Évora":
        return "Evora";
      case "Faro":
        return "Faro";
      case "Guarda":
        return "Guarda";
      case "Leiria":
        return "Leiria";
      case "Lisboa":
        return "Lisboa";
      case "Portalegre":
        return "Portalegre";
      case "Porto":
        return "Porto";
      case "Santarém":
        return "Santarem";
      case "Setúbal":
        return "Setubal";
      case "Viana do Castelo":
        return "VianaDoCastelo";
      case "Vila Real":
        return "VilaReal";
      case "Viseu":
        return "Viseu";
      default:
        return "";
    }
  }

  private static String districtNameDecoder(String district) {
    switch (district) {
      case "Aveiro":
        return "Aveiro";
      case "Beja":
        return "Beja";
      case "Braga":
        return "Braga";
      case "Braganca":
        return "Bragança";
      case "CasteloBranco":
        return "Castelo Branco";
      case "Coimbra":
        return "Coimbra";
      case "Evora":
        return "Évora";
      case "Faro":
        return "Faro";
      case "Guarda":
        return "Guarda";
      case "Leiria":
        return "Leiria";
      case "Lisboa":
        return "Lisboa";
      case "Portalegre":
        return "Portalegre";
      case "Porto":
        return "Porto";
      case "Santarem":
        return "Santarém";
      case "Setubal":
        return "Setúbal";
      case "VianaDoCastelo":
        return "Viana do Castelo";
      case "VilaReal":
        return "Vila Real";
      case "Viseu":
        return "Viseu";
      default:
        return "";
    }
  }

  private static String inputStreamToString(InputStream stream){
    StringBuilder textBuilder = new StringBuilder();
    try (Reader reader = new BufferedReader(new InputStreamReader
      (stream))) {
      int c = 0;
      while ((c = reader.read()) != -1) {
        textBuilder.append((char) c);
      }
    } catch (IOException e) {
      e.printStackTrace();
    }
    return textBuilder.toString();
  }

  private static HttpURLConnection makeRequest(String endpoint) throws IOException {
    URL endpointURL = new URL(baseURL + endpoint);

    System.out.println(endpointURL.toString());
    HttpURLConnection conn = (HttpURLConnection) endpointURL.openConnection();
    if (conn.getResponseCode() != 200) {
      throw new RuntimeException("Failed : HTTP Error code : "
        + conn.getResponseCode());
    }
    return conn;
  }

  public static int getNumberOfUsersInDistrict(String district) throws IOException {
    HttpURLConnection conn = makeRequest("/district/getTotalUsers?district=" + districtNameEncoder(district));
    return Integer.parseInt(inputStreamToString(conn.getInputStream()));
  }

  public static int getNumberOfInfectedUsersInDistrict(String district) throws IOException {
    HttpURLConnection conn = makeRequest("/district/getTotalInfected?district=" + districtNameEncoder(district));
    return Integer.parseInt(inputStreamToString(conn.getInputStream()));
  }

  public static float getAverageNumberOfUserThatHadContactWithPatients() throws IOException {
    HttpURLConnection conn = makeRequest("/getUsersMeetInfected");
    return Float.parseFloat(inputStreamToString(conn.getInputStream()));
  }

  public static String[] getRacioList() throws IOException {
    HttpURLConnection conn = makeRequest("/getRacioMostInfected");
    JSONArray resJSON = new JSONArray(inputStreamToString(conn.getInputStream()));
    String[] output = new String[resJSON.length()];
    JSONObject object;
    for (int i = 0; i < resJSON.length() ; i++) {
      object = resJSON.getJSONObject(i);
      output[i] = districtNameDecoder(object.getString("nameLocation")) + ": " + object.getFloat("ratio");
    }
    return output;
  }

  public static String[] getMostCrowded() throws IOException {
    HttpURLConnection conn = makeRequest("/getMostCrowded");
    JSONArray resJSON = new JSONArray(inputStreamToString(conn.getInputStream()));
    String[] output = new String[resJSON.length()];
    JSONObject object;
    for (int i = 0; i < resJSON.length() ; i++) {
      object = resJSON.getJSONObject(i);
      output[i] = "Location: " + object.getString("nameLocation") + " | nº: " + object.getInt("nusers");
    }
    return output;
  }

}
