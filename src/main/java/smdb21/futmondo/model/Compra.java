package smdb21.futmondo.model;

import java.text.ParseException;
import java.util.Date;

import smdb21.futmondo.Main;

public class Compra {
	private final String comprador;
	private final String vendedor;
	private final String jugador;
	private final int precio;
	private final Date fecha;
	private final TipoCompra tipoCompra;
	private final String photoJugadorURL;

	public Compra(String comprador, String vendedor, String jugador, int precio, TipoCompra tipoCompra, Date fecha,
			String photoJugadorURL) {
		super();
		this.comprador = comprador;
		this.vendedor = vendedor;
		this.jugador = jugador;
		this.precio = precio;
		this.tipoCompra = tipoCompra;
		this.fecha = fecha;
		this.photoJugadorURL = photoJugadorURL;
	}

	public String getComprador() {
		return comprador;
	}

	public String getVendedor() {
		return vendedor;
	}

	public String getJugador() {
		return jugador;
	}

	public int getPrecio() {
		return precio;
	}

	public TipoCompra getTipoCompra() {
		return tipoCompra;
	}

	public String getPhotoJugadorURL() {
		return photoJugadorURL;
	}

	@Override
	public String toString() {
		return Main.formatDate.format(fecha) + "\t" + comprador + "\t" + vendedor + "\t" + jugador + "\t" + precio
				+ "\t" + tipoCompra + "\t" + photoJugadorURL;
	}

	public static String headerString() {
		return "Fecha\tComprador\tVendedor\tJugador\tPrecio\tTipo\tFoto";
	}

	public static Compra fromLine(String string) throws ParseException {
		final String[] split = string.split("\t");
		final Date fecha = Main.formatDate.parse(split[0]);
		final String comprador = split[1];
		final String vendedor = split[2];
		final String jugador = split[3];
		final int precio = Integer.valueOf(split[4]);
		final TipoCompra tipoCompra = TipoCompra.valueOf(split[5]);
		String photoJugadorURL = null;
		if (split.length > 6 && !"null".equals(split[6])) {
			photoJugadorURL = split[6];
		}
		final Compra compra = new Compra(comprador, vendedor, jugador, precio, tipoCompra, fecha, photoJugadorURL);
		return compra;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((comprador == null) ? 0 : comprador.hashCode());
		result = prime * result + ((fecha == null) ? 0 : fecha.hashCode());
		result = prime * result + ((jugador == null) ? 0 : jugador.hashCode());
		result = prime * result + precio;
		result = prime * result + ((tipoCompra == null) ? 0 : tipoCompra.hashCode());
		result = prime * result + ((vendedor == null) ? 0 : vendedor.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final Compra other = (Compra) obj;
		if (comprador == null) {
			if (other.comprador != null)
				return false;
		} else if (!comprador.equals(other.comprador))
			return false;
		if (fecha == null) {
			if (other.fecha != null)
				return false;
		} else if (!fecha.equals(other.fecha))
			return false;
		if (jugador == null) {
			if (other.jugador != null)
				return false;
		} else if (!jugador.equals(other.jugador))
			return false;
		if (precio != other.precio)
			return false;
		if (tipoCompra != other.tipoCompra)
			return false;
		if (vendedor == null) {
			if (other.vendedor != null)
				return false;
		} else if (!vendedor.equals(other.vendedor))
			return false;
		return true;
	}

}
