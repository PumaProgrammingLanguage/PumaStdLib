#include "pch.h"
#include "framework.h"
#include "Directory.hpp"
#include <filesystem>
#include <string>
#include <algorithm>

using namespace std;

namespace PumaFile
{
	namespace Directory
	{
		// Get current working directory - normalized to forward slashes
        PumaType::String GetCurrentDirectory() noexcept
		{
			error_code ec;
			const filesystem::path path = filesystem::current_path(ec);
			// On error, return empty string
			if (ec)
			{
				return PumaType::String();
			}

			// Normalize to forward slashes - Puma does not support backslashes
			string normalizedPath = path.string();
			replace(normalizedPath.begin(), normalizedPath.end(), '\\', '/');
          // Return as PumaType::String
			return PumaType::String(normalizedPath.data(), normalizedPath.size());
		}

		// Set current working directory - normalized to forward slashes
		bool SetCurrentDirectory(const PumaType::String& path) noexcept
		{
			const uint32_t pathSize = path.Size();
			// Empty path is invalid
			if (pathSize == 0)
			{
				return false;
			}
			// All modern day OSes support forward slashes, so normalize for portable code
			string native(path.ToUTF8(), path.ToUTF8() + path.Size());
			replace(native.begin(), native.end(), '\\', '/');
			// Set current path
			error_code ec;
			filesystem::current_path(native, ec);
			// Return success status
			return !ec;
		}
	} // namespace Directory
} // namespace PumaFile