package client;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQException;


public class PubNotificationsThread extends Thread {

  private final ZMQ.Socket socket;
  private Controller controller;


  public PubNotificationsThread(Controller controller) {
    socket = new ZContext().createSocket(SocketType.SUB);
    for (int i = 1; i <= 18; ++i)
      socket.connect("tcp://localhost:7" + String.format("%02d", i) + "2");
    this.controller = controller;
  }

  public void subscribe(int distNum) {
    socket.subscribe(String.format("%02d ", distNum));
  }

  public void unsubscribe(int distNum) {
    socket.unsubscribe(String.format("%02d ", distNum));
  }

  @Override
  public void run() {
    try {
      while (true) {
        try {
          String[] message = socket.recvStr().split(" ", 2);
          int district = Integer.parseInt(message[0]);
          System.out.println(message[0] + message[1]);
          String msg = message[1];
          controller.newWarning(decodeDistrict(district) + ": " + msg);
        } catch (ZMQException e) {
          break;
        }
      }
    } finally {
      socket.close();
    }
  }

  public String decodeDistrict(int districtNumber) {
    switch (districtNumber) {
      case 1:
        return "Aveiro";
      case 2:
        return "Beja";
      case 3:
        return "Braga";
      case 4:
        return "Bragança";
      case 5:
        return "Castelo Branco";
      case 6:
        return "Coimbra";
      case 7:
        return "Évora";
      case 8:
        return "Faro";
      case 9:
        return "Guarda";
      case 10:
        return "Leiria";
      case 11:
        return "Lisboa";
      case 12:
        return "Portalegre";
      case 13:
        return "Porto";
      case 14:
        return "Santarém";
      case 15:
        return "Setúbal";
      case 16:
        return "Viana do Castelo";
      case 17:
        return "Vila Real";
      case 18:
        return "Viseu";
      default:
        return "ERRO";
    }
  }

}
