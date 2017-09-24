namespace PromotionUtils
{
	public class PromotionMessage
	{
		private
			string fMessagePath;

		public PromotionMessage( string messagePath )
		{
			streamreader f = new file().openText( messagePath );
			fMessagePath = f.Read
		}

		public 
			string produceText( string userName, string serial, string email, string password )
			{
			}
	}
}
