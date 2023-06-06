package cz.cvut.fit.niam1.messaging;


import java.io.FileOutputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import java.util.stream.Collectors;

import javax.management.MBeanServerConnection;
import javax.management.MBeanServerInvocationHandler;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

import org.apache.activemq.broker.jmx.BrokerViewMBean;
import org.apache.activemq.broker.jmx.QueueViewMBean;

public class QueueStats {

    public final static String STATS_OUTPUT = "data_" + (new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").format(new Date()))
            + ".csv";

    public JMXConnector jmxCon = null;
    public MBeanServerConnection connection = null;

    public void open() throws Exception {

        System.out.println("Connecting...");

        connection = JMXConnectorFactory
                .connect(new JMXServiceURL("service:jmx:rmi:///jndi/rmi://localhost:1099/jmxrmi"))
                .getMBeanServerConnection();

    }

    public void close() throws Exception {
        if (jmxCon != null) {
            jmxCon.close();
        }
    }

    public List<List<String>> readStats() throws Exception {

        List<List<String>> out = new ArrayList<>();

        String jmsServerName = "ActiveMQ";
        ObjectName activeMQ = new ObjectName("org.apache.activemq:type=Broker,brokerName=localhost");

        BrokerViewMBean mbean = (BrokerViewMBean) MBeanServerInvocationHandler.newProxyInstance(connection, activeMQ,
                BrokerViewMBean.class, true);

        for (ObjectName name : mbean.getQueues()) {
            String destinationName = name.getKeyProperty("destinationName");
            QueueViewMBean queueMbean = (QueueViewMBean) MBeanServerInvocationHandler.newProxyInstance(connection, name,
                    QueueViewMBean.class, true);

            System.out.println(destinationName);

            long messagesCurrentCount = (Long) queueMbean.getQueueSize();
            System.out.println("messagesCurrentCount " + messagesCurrentCount);

            long messagesPendingCount = (Long) queueMbean.getInFlightCount();
            System.out.println("messagesPendingCount " + messagesPendingCount);

            long messagesReceivedCount = (Long) queueMbean.getEnqueueCount();
            System.out.println("messagesReceivedCount " + messagesReceivedCount);

            long messagesHighCount = -1;
            System.out.println("messagesHighCount " + messagesHighCount);

            System.out.println("--------------------------------------------------");
//           build an entry
            ArrayList<String> entry = new ArrayList<>();
            entry.add(new SimpleDateFormat("yyyy.MM.dd HH:mm:ss").format(new Date()));
            entry.add(jmsServerName);
            entry.add(destinationName);
            entry.add(String.valueOf(messagesCurrentCount));
            entry.add(String.valueOf(messagesPendingCount));
            entry.add(String.valueOf(messagesReceivedCount));
            entry.add(String.valueOf(messagesHighCount));
            out.add(entry);

        }

        return (out);
    }

    public static void main(String[] args) throws Exception {

        FileOutputStream fos = new FileOutputStream(STATS_OUTPUT, false);
        fos.write(
                "timestamp,server,queue,messagesCurrentCount,messagesPendingCount,messagesReceivedCount,messagesHighCount\n"
                        .getBytes());
        fos.close();

        QueueStats stats = new QueueStats();
        stats.open();

        Timer t = new Timer();
        t.schedule(new TimerTask() {
            @Override
            public void run() {
                try {
                    FileOutputStream fos = new FileOutputStream(STATS_OUTPUT, true);
                    String csvStats = stats.readStats().stream().map(entry -> String.join(",", entry))
                            .collect(Collectors.joining("\n")) + "\n";
                    fos.write(csvStats.getBytes());
                    fos.close();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }, 0, 5000);

//		stats.close();

    }

}