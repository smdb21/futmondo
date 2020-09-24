package smdb21.futmondo.model;

public class PuntosEnJornada {
	private final int jornada;
	private final String jugador;
	private final int posicion;
	private final double puntos;

	public PuntosEnJornada(int jornada, String jugador, int posicion, double puntos) {
		this.jornada = jornada;
		this.jugador = jugador;
		this.posicion = posicion;
		this.puntos = puntos;
	}

	public int getJornada() {
		return jornada;
	}

	public String getJugador() {
		return jugador;
	}

	public int getPosicion() {
		return posicion;
	}

	public double getPuntos() {
		return puntos;
	}

	public static String headerString() {
		return "jornada\tjugador\tposicion\tpuntos";
	}

	@Override
	public String toString() {
		return jornada + "\t" + jugador + "\t" + posicion + "\t" + puntos;
	}
}
