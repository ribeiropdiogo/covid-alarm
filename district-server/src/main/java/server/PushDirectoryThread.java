package server;



public class PushDirectoryThread extends Thread{
    private ExecutionThread et;
    private String address;


    public PushDirectoryThread(ExecutionThread et, String address){
        this.et = et;
        this.address = address;
    }


    @Override
    public void run() {
        while(true) {
            //this thread will update district's information every 30 seconds
            try {
                et.upDateDistrict(address);
                System.out.println("Thread is doing something");
                Thread.sleep(30000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}
